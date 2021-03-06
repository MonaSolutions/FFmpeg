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
#include "mona.h"

typedef struct WGContext {
    const AVClass *	class;
    broadcaster*	publication;
	WGConfig		config;
	AVIOContext *	write_pb;
	unsigned int	write_size; // current packet size
	uint8_t *		audio_config;
	int				audio_size;
	uint8_t *		video_config;
	int				video_size;
	uint8_t *		properties;
	int				properties_size;

} WGContext;

#define D AV_OPT_FLAG_DECODING_PARAM
#define E AV_OPT_FLAG_ENCODING_PARAM
#define OFFSET(x) offsetof(WGContext, x)
static const AVOption libwg_options[] = {
	{ "wss",            "Enable WSS port",      OFFSET(config.ssl),        AV_OPT_TYPE_INT, { .i64 = 0 }, 0, 1, .flags = D|E },
    { NULL }
};

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

	wg_start(&c->config);
	if (!(c->publication = wg_broadcast(path+1, c->config.ssl)))
		return AVERROR_UNKNOWN;
	return 0;
}

/**
 * @param[in,out] read_pb pointer to the reading buffer
 * @param[in] buf pointer to the buffer to write (size is in c->write_size)
 * @param[in] ref buffer is passed by reference and will be destroyed by libwebgroup
 * @return size written, 0 if nothing happend (nothing to flush), <0 if an error occured
 */
static int libwg_broadcast(URLContext *h, AVIOContext* read_pb, uint8_t *buf, unsigned short ref) {
	WGContext *c = h->priv_data;
	int frame = 0, type = (c->write_size > 1) ? (buf[0] >> 6) : 0;
	int ret = 0;
	if (type == MONA_TYPE_VIDEO)
		frame = buf[1] >> 3;
	
	if (!type && c->write_size > 1 && ((buf[0] & 0x3F) == MONA_TYPE_JSON)) { // metadatas properties
		// save metadatas
		if (c->properties)
			av_free(c->properties);
		c->properties_size = c->write_size;
		c->properties = av_malloc(c->properties_size);
		memcpy(c->properties, buf, c->properties_size);
		if (ref)
			av_free(buf); // release buffer before exiting
		av_log(h, AV_LOG_INFO, "Properties received : %.*s\n", c->write_size, (char*)buf+1);
	}
	else if (type == MONA_TYPE_AUDIO && buf[2] & 2) {
		// save audio config
		if (c->audio_config)
			av_free(c->audio_config);
		c->audio_size = c->write_size;
		c->audio_config = av_malloc(c->audio_size);
		memcpy(c->audio_config, buf, c->audio_size);
		if (ref)
			av_free(buf); // release buffer before exiting
		av_log(h, AV_LOG_DEBUG, "Audio config received, size : %d\n", c->audio_size);
	}
	else if (frame == MONA_FRAME_CONFIG) {
		// save video config
		if (c->video_config)
			av_free(c->video_config);
		c->video_size = c->write_size;
		c->video_config = av_malloc(c->video_size);
		memcpy(c->video_config, buf, c->video_size);
		if (ref)
			av_free(buf); // release buffer before exiting
		av_log(h, AV_LOG_DEBUG, "Video config received, size : %d\n", c->video_size);
	}
	else {
		//av_log(h, AV_LOG_INFO, "type : %d, frame : %d, size : %d, \n", type, frame, c->write_size);

		// If the packet is a Video Key frame we send the config packets and set isKey to True
		if (frame == MONA_FRAME_KEY) {
			unsigned short is_key = 1;
			if (c->properties) {
				if (!wg_broadcast_write(c->publication, c->properties, c->properties_size, is_key))
					ret = AVERROR_UNKNOWN;
				is_key = 0;
			}
			if (ret >= 0 && c->video_config) {
				if (!wg_broadcast_write(c->publication, c->video_config, c->video_size, is_key))
					ret = AVERROR_UNKNOWN;
				is_key = 0;
			}
			if (ret >= 0 && c->audio_config) {
				if (!wg_broadcast_write(c->publication, c->audio_config, c->audio_size, is_key))
					ret = AVERROR_UNKNOWN;
				is_key = 0;
			}
			if (ret < 0) {
				if (ref)
					av_free(buf); // release buffer before exiting
			} else if (!wg_broadcast_write(c->publication, buf, (ref ? -c->write_size : c->write_size), is_key))
				ret = AVERROR_UNKNOWN;
		}
		else {
			if ((type == MONA_TYPE_VIDEO && !c->video_config) || (type == MONA_TYPE_AUDIO && !c->audio_config)) {
				av_log(h, AV_LOG_WARNING, "%s Packet ignored, no config received for now\n", (type == MONA_TYPE_VIDEO) ? "Video" : "Audio");
				if (ref)
					av_free(buf); // release buffer before exiting
			} else if (!wg_broadcast_write(c->publication, buf, (ref ? -c->write_size : c->write_size), 0))
				ret = AVERROR_UNKNOWN;
		}
	}
	
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

	// buffer started < 4 (size not read)
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
		c->write_pb = 0;
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
			if (to_read > available) { // bufferize again
				avio_write(c->write_pb, read_pb.buf_ptr, available);
				break;
			}

			// Grab the buffer and broadcast
			avio_write(c->write_pb, read_pb.buf_ptr, to_read);
			avio_close_dyn_buf(c->write_pb, &write_buffer);
			flush += (ret = libwg_broadcast(h, &read_pb, write_buffer, 1));

			// progress reading
			avio_skip(&read_pb, to_read);
			available -= to_read;
			c->write_pb = 0;
			c->write_size = 0;
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

			flush += (ret = libwg_broadcast(h, &read_pb, read_pb.buf_ptr, 0));

			// progress reading
			avio_skip(&read_pb, c->write_size);
			available -= c->write_size; 
			c->write_size = 0;
		}
		if (ret < 0)
			return ret;
	}
	if (flush)
		wg_broadcast_flush(c->publication, 0);
    return size; // always read all
}

static int libwg_close(URLContext *h)
{
    WGContext *c = h->priv_data;

	wg_broadcast_flush(c->publication, 1);
	wg_broadcast_close(c->publication);
	wg_stop();

	// Empty buffers
	if (c->write_pb) {
		uint8_t * write_buffer;
		avio_close_dyn_buf(c->write_pb, &write_buffer);
		c->write_pb = 0;
		av_free(write_buffer);
	}
	if (c->audio_config)
		av_free(c->audio_config);
	if (c->video_config)
		av_free(c->video_config);

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
    .url_write           = libwg_write,
    .url_close           = libwg_close,
    .priv_data_size      = sizeof(struct WGContext),
    .flags               = URL_PROTOCOL_FLAG_NETWORK,
    .priv_data_class     = &libwg_class,
};
