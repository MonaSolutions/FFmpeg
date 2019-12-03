/*
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * Haivision WebGroup protocol
 */

#include <libwebgroup.h>

#include "libavutil/avassert.h"
#include "libavutil/opt.h"
#include "libavutil/parseutils.h"
#include "libavutil/time.h"

#include "avformat.h"
#include "internal.h"
#include "network.h"
#include "os_support.h"
#include "url.h"
#include "avio_internal.h"

typedef struct WGContext {
    const AVClass *	class;
    broadcaster*	publication;
	WGConfig		config;
	AVIOContext *	write_pb;
	unsigned int	write_size; // current packet size

} WGContext;

#define OFFSET(x) offsetof(WGContext, x)
static const AVOption libwg_options[] = {
	// TODO: add WSS option
    { NULL }
};

static int libwg_wait_start(URLContext *h, const char* path, int64_t timeout)
{
	WGContext *c = h->priv_data;
	int64_t wait_start = 0;
	int ret = 0;

	wg_start(&c->config);
	while (1) {
		if ((c->publication = wg_broadcast(path)) != 0)
			return 0;

		if (ret = av_usleep(POLLING_TIME))
			return ret;

		if (ff_check_interrupt(&h->interrupt_callback))
			return AVERROR_EXIT;
		if (timeout > 0) {
			if (!wait_start)
				wait_start = av_gettime_relative();
			else if (av_gettime_relative() - wait_start > timeout)
				return AVERROR(ETIMEDOUT);
		}
	}
}

static int libwg_open(URLContext *h, const char *uri, int flags)
{
	char proto[8], path[1024], auth[100];
	WGContext *c = h->priv_data;

	av_url_split(proto, sizeof(proto), auth, sizeof(auth),
		c->config.hostname, sizeof(c->config.hostname), &c->config.port,
		path, sizeof(path), h->filename);
	if (c->config.port == -1)
		c->config.port = 0; // reset port value if not set
	if (!strlen(path))
		return AVERROR(EINVAL);

	return libwg_wait_start(h, path+1, h->rw_timeout); // (+1 to remove '/')
}

/*static int libwg_read(URLContext *h, uint8_t *buf, int size)
{
    WGContext *c = h->priv_data;
    int ret;

    ret = srt_recvmsg(s->fd, buf, size);
    if (ret < 0) {
        ret = libsrt_neterrno(h);
    }

    return ret;
}*/

static int libwg_broadcast(URLContext *h, AVIOContext* read_pb, uint8_t *buf, int* available, int to_read) {
	WGContext *c = h->priv_data;
	int frame = 0, type = (c->write_size > 1) ? (buf[0] >> 6) : 0;
	int ret;
	if (type == 3) // video
		frame = buf[1] >> 3;

	//av_log(h, AV_LOG_INFO, "type : %d, frame : %d, size : %d, \n", type, frame, c->write_size);
	ret = wg_broadcast_write(c->publication, buf, c->write_size, frame == 7);
	c->write_pb = 0;

	// progress reading
	avio_skip(read_pb, to_read);
	*available -= to_read;
	c->write_size = 0;

	return ret;
}

static int libwg_write(URLContext *h, const uint8_t *buf, int size)
{
    WGContext *c = h->priv_data;
	AVIOContext read_pb;
	int ret, flush = 0;
	int available=size;
	if ((ret = ffio_init_context(&read_pb, (unsigned char *)buf, size, 0,
		NULL, NULL, NULL, NULL)) < 0)
		return ret;

	// buffer started and size not read 
	if (c->write_pb && !c->write_size) {
		uint8_t * write_buffer;
		int64_t to_read = 4 - avio_size(c->write_pb);
		if (available < to_read) {
			av_log(h, AV_LOG_ERROR, "Not enough data to read the size of payload (%d < %ld)\n", available, to_read);
			return AVERROR(EINVAL);
		}
		avio_write(c->write_pb, read_pb.buf_ptr, to_read);
		c->write_size = avio_rb32(c->write_pb); // read size TODO: test this
		av_log(h, AV_LOG_WARNING, "Read size : %d\n", c->write_size);

		// close the current buffer and progress in the buffer
		avio_close_dyn_buf(c->write_pb, &write_buffer);
		av_free(write_buffer);
		avio_skip(&read_pb, to_read);
		available -= to_read;
	}

	while (available) {
		// continue current buffering
		if (c->write_pb) {
			uint8_t * write_buffer;
			int written = avio_get_dyn_buf(c->write_pb, &write_buffer);
			int64_t to_read = c->write_size - written;
			if (to_read > available) {
				avio_write(c->write_pb, read_pb.buf_ptr, available);
				break;
			}

			// Grab the buffer and broadcast
			avio_write(c->write_pb, read_pb.buf_ptr, to_read);
			avio_close_dyn_buf(c->write_pb, &write_buffer);
			
			flush += (ret = libwg_broadcast(h, &read_pb, write_buffer, &available, to_read));
			av_free(write_buffer); // release buffer before exiting
			if (!ret)
				return AVERROR_UNKNOWN;
		}
		else {

			if (available < 4) { // not enough data to read the size
				if ((ret = avio_open_dyn_buf(&c->write_pb)) < 0)
					return ret;
				avio_write(c->write_pb, read_pb.buf_ptr, available);
				break;
			}
			c->write_size = avio_rb32(&read_pb);
			available -= 4;
			if (c->write_size > available) {
				if ((ret = avio_open_dyn_buf(&c->write_pb)) < 0)
					return ret;
				avio_write(c->write_pb, read_pb.buf_ptr, available);
				break;
			}

			flush += (ret = libwg_broadcast(h, &read_pb, read_pb.buf_ptr, &available, c->write_size));
			if (!ret)
				return AVERROR_UNKNOWN;
		}
	}
	if (flush)
		wg_broadcast_flush(c->publication, 0);
    return size;
}

static int libwg_close(URLContext *h)
{
    WGContext *c = h->priv_data;

	wg_broadcast_flush(c->publication, 1);
	wg_broadcast_close(c->publication);

	wg_stop();

	// Empty write buffer
	if (c->write_pb) {
		uint8_t * write_buffer;
		avio_close_dyn_buf(c->write_pb, &write_buffer);
		av_free(write_buffer);
	}

    return 0;
}

static const AVClass libwg_class = {
    .class_name = "libwebgroup",
    .item_name  = av_default_item_name,
    .option     = libwg_options,
    .version    = LIBAVUTIL_VERSION_INT,
};

const URLProtocol ff_libwebgroup_protocol = {
    .name                = "wg",
    .url_open            = libwg_open,
//    .url_read            = libwg_read,
    .url_write           = libwg_write,
    .url_close           = libwg_close,
    .priv_data_size      = sizeof(struct WGContext),
    .flags               = URL_PROTOCOL_FLAG_NETWORK,
    .priv_data_class     = &libwg_class,
};
