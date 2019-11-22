/*
 * FLV muxer
 * Copyright (c) 2003 The FFmpeg Project
 *
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

#include "libavutil/intreadwrite.h"
#include "libavutil/dict.h"
#include "libavutil/intfloat.h"
#include "libavutil/avassert.h"
#include "libavutil/mathematics.h"
#include "avio_internal.h"
#include "avio.h"
#include "avc.h"
#include "avformat.h"
#include "internal.h"
#include "metadata.h"
#include "libavutil/opt.h"
#include "libavcodec/put_bits.h"
#include "libavcodec/aacenctab.h"


static const AVCodecTag mona_video_codec_ids[] = {
    { AV_CODEC_ID_FLV1,     FLV_CODECID_H263 },
    { AV_CODEC_ID_H263,     FLV_CODECID_REALH263 },
    { AV_CODEC_ID_MPEG4,    FLV_CODECID_MPEG4 },
    { AV_CODEC_ID_FLASHSV,  FLV_CODECID_SCREEN },
    { AV_CODEC_ID_FLASHSV2, FLV_CODECID_SCREEN2 },
    { AV_CODEC_ID_VP6F,     FLV_CODECID_VP6 },
    { AV_CODEC_ID_VP6,      FLV_CODECID_VP6 },
    { AV_CODEC_ID_VP6A,     FLV_CODECID_VP6A },
    { AV_CODEC_ID_H264,     FLV_CODECID_H264 },
    { AV_CODEC_ID_NONE,     0 }
};

static const AVCodecTag mona_audio_codec_ids[] = {
    { AV_CODEC_ID_MP3,        FLV_CODECID_MP3        >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_PCM_U8,     FLV_CODECID_PCM        >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_PCM_S16BE,  FLV_CODECID_PCM        >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_PCM_S16LE,  FLV_CODECID_PCM_LE     >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_ADPCM_SWF,  FLV_CODECID_ADPCM      >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_AAC,        FLV_CODECID_AAC        >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_NELLYMOSER, FLV_CODECID_NELLYMOSER >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_PCM_MULAW,  FLV_CODECID_PCM_MULAW  >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_PCM_ALAW,   FLV_CODECID_PCM_ALAW   >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_SPEEX,      FLV_CODECID_SPEEX      >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_NONE,       0 }
};

typedef struct MonaContext {
    AVClass *av_class;
    
} MonaContext;

static void mona_write_codec_header(AVFormatContext* s, AVCodecParameters* par, int64_t ts) {
    AVIOContext *pb = s->pb;
    MonaContext *flv = s->priv_data;

   
}

static int mona_init(struct AVFormatContext *s)
{
    int i;
	MonaContext *flv = s->priv_data;

    return 0;
}

static int mona_write_header(AVFormatContext *s)
{
    AVIOContext *pb = s->pb;
    MonaContext *flv = s->priv_data;

   
    return 0;
}

static int mona_write_trailer(AVFormatContext *s)
{
    AVIOContext *pb = s->pb;
    MonaContext *flv = s->priv_data;

    return 0;
}

static int mona_write_packet(AVFormatContext *s, AVPacket *pkt)
{
    AVIOContext *pb      = s->pb;
    AVCodecParameters *par = s->streams[pkt->stream_index]->codecpar;
    MonaContext *flv      = s->priv_data;

    return 0;
}

static int mona_check_bitstream(struct AVFormatContext *s, const AVPacket *pkt)
{
    int ret = 1;
    
    return ret;
}

static const AVOption options[] = {
    { NULL },
};

static const AVClass mona_muxer_class = {
    .class_name = "mona muxer",
    .item_name  = av_default_item_name,
    .option     = options,
    .version    = LIBAVUTIL_VERSION_INT,
};

AVOutputFormat ff_mona_muxer = {
    .name           = "mona",
    .long_name      = NULL_IF_CONFIG_SMALL("MONA"),
    .mime_type      = "video/mona",
    .extensions     = "mona",
    .priv_data_size = sizeof(MonaContext),
    .audio_codec    = CONFIG_LIBMP3LAME ? AV_CODEC_ID_MP3 : AV_CODEC_ID_ADPCM_SWF,
    .video_codec    = AV_CODEC_ID_FLV1,
    .init           = mona_init,
    .write_header   = mona_write_header,
    .write_packet   = mona_write_packet,
    .write_trailer  = mona_write_trailer,
    .check_bitstream= mona_check_bitstream,
    .codec_tag      = (const AVCodecTag* const []) {
                          mona_video_codec_ids, mona_audio_codec_ids, 0
                      },
    .flags          = AVFMT_GLOBALHEADER | AVFMT_VARIABLE_FPS |
                      AVFMT_TS_NONSTRICT,
    .priv_class     = &mona_muxer_class,
};
