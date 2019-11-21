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

typedef struct WGContext {
    const AVClass *class;
    unsigned int publication;

} WGContext;

#define OFFSET(x) offsetof(WGContext, x)
static const AVOption libwg_options[] = {
    { NULL }
};

static int libwg_open(URLContext *h, const char *uri, int flags)
{
    WGContext *c = h->priv_data;
    const char * p;
    char buf[256];
    int ret = 0;

    if (wg_open(NULL) == 0) {
        return AVERROR_UNKNOWN;
    }

    if ((c->publication = wg_publish(h->filename))==0)
        return AVERROR_UNKNOWN;
    return 0;
}

/*static int libwg_read(URLContext *h, uint8_t *buf, int size)
{
    WGContext *c = h->priv_data;
    int ret;

    if (!(h->flags & AVIO_FLAG_NONBLOCK)) {
        ret = libsrt_network_wait_fd_timeout(h, s->eid, s->fd, 0, h->rw_timeout, &h->interrupt_callback);
        if (ret)
            return ret;
    }

    ret = srt_recvmsg(s->fd, buf, size);
    if (ret < 0) {
        ret = libsrt_neterrno(h);
    }

    return ret;
}*/

static int libwg_write(URLContext *h, const uint8_t *buf, int size)
{
    WGContext *c = h->priv_data;
    int ret;
    Tag tag;

    /*if (!(h->flags & AVIO_FLAG_NONBLOCK)) {
        ret = libsrt_network_wait_fd_timeout(h, s->eid, s->fd, 1, h->rw_timeout, &h->interrupt_callback);
        if (ret)
            return ret;
    }*/

    av_log(h, AV_LOG_INFO, "size : %d\n", size); 
    ret = wg_publish_media(c->publication, &tag, buf, size);
    return size;
}

static int libwg_close(URLContext *h)
{
    WGContext *c = h->priv_data;

    wg_publish_close(c->publication);

    wg_close();

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
