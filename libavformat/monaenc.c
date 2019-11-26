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

#define MONA_CODEC_RAW				0
#define FLV_AUDIO_CODECID_OFFSET	4

enum { // Keep UInt8 to allow easly a compression + String::ToNumber usage!
	MONA_TYPE_NONE = 0,
	MONA_TYPE_DATA = 1,
	MONA_TYPE_AUDIO = 2, // => 10, to have the first bit to 1 and be compatible with Media::Pack
	MONA_TYPE_VIDEO = 3, // => 11, to have the first bit to 1 and be compatible with Media::Pack
	// these values allow to write media type on 2 bits!
};

enum {  // Keep UInt8 to allow easly a compression + String::ToNumber usage!
	// Aligned values with FLV frame type value, and ignore FRAME_GENERATED_KEYFRAME which is redundant with FRAME_KEY for a "key frame" test condition
	MONA_FRAME_UNSPECIFIED = 0,
	MONA_FRAME_KEY = 1,
	MONA_FRAME_INTER = 2, // Used too by H264 for Config sequence
	MONA_FRAME_DISPOSABLE_INTER = 3, // just for H263
	MONA_FRAME_INFO = 5,
	MONA_FRAME_CONFIG = 7
};

enum {
	// Aligned values with FLV codec value
	MONA_CODEC_JPEG = 1,
	MONA_CODEC_SORENSON = 2,
	MONA_CODEC_SCREEN1 = 3,
	MONA_CODEC_VP6 = 4,
	MONA_CODEC_VP6_ALPHA = 5,
	MONA_CODEC_SCREEN2 = 6,
	MONA_CODEC_H264 = 7,
	MONA_CODEC_H263 = 8,
	MONA_CODEC_MPEG4_2 = 9,
	MONA_CODEC_HEVC = 12 // Added codec HEVC (not officialy supported by FLV)
	// FFmpeg fork with hevc support in flv : https://github.com/ksvc/FFmpeg/wiki
};

enum {
	// Aligned values with FLV codec value
	MONA_CODEC_ADPCM = 1,
	MONA_CODEC_MP3 = 2,
	MONA_CODEC_PCM_LITTLE = 3,
	MONA_CODEC_NELLYMOSER_16K = 4,
	MONA_CODEC_NELLYMOSER_8K = 5,
	MONA_CODEC_NELLYMOSER = 6,
	MONA_CODEC_G711A = 7,
	MONA_CODEC_G711U = 8,
	MONA_CODEC_AAC = 10,
	MONA_CODEC_SPEEX = 11,
	MONA_CODEC_MP38K_FLV = 14 // just usefull for FLV!
};

static const AVCodecTag mona_video_codec_ids[] = {
    { AV_CODEC_ID_FLV1,     MONA_CODEC_SORENSON },
    { AV_CODEC_ID_H263,     MONA_CODEC_H263 },
    { AV_CODEC_ID_MPEG4,    MONA_CODEC_MPEG4_2 },
    { AV_CODEC_ID_FLASHSV,  MONA_CODEC_SCREEN1 },
    { AV_CODEC_ID_FLASHSV2, MONA_CODEC_SCREEN2 },
    { AV_CODEC_ID_VP6F,     MONA_CODEC_VP6 },
    { AV_CODEC_ID_VP6,      MONA_CODEC_VP6 },
    { AV_CODEC_ID_VP6A,     MONA_CODEC_VP6_ALPHA },
    { AV_CODEC_ID_H264,     MONA_CODEC_H264 },
    { AV_CODEC_ID_NONE,     0 }
};

static const AVCodecTag mona_audio_codec_ids[] = {
    { AV_CODEC_ID_MP3,        MONA_CODEC_MP3 >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_PCM_U8,     MONA_CODEC_RAW >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_PCM_S16BE,  MONA_CODEC_RAW >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_PCM_S16LE,  MONA_CODEC_PCM_LITTLE >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_ADPCM_SWF,  MONA_CODEC_ADPCM >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_AAC,        MONA_CODEC_AAC >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_NELLYMOSER, MONA_CODEC_NELLYMOSER >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_PCM_MULAW,  MONA_CODEC_G711U >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_PCM_ALAW,   MONA_CODEC_G711A >> FLV_AUDIO_CODECID_OFFSET },
    { AV_CODEC_ID_SPEEX,      MONA_CODEC_SPEEX >> FLV_AUDIO_CODECID_OFFSET },
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

static void mona_write_video_header(AVFormatContext *s, AVPacket *pkt) {
	AVIOContext *pb = s->pb;
	AVCodecParameters *par = s->streams[pkt->stream_index]->codecpar;
	unsigned int frame = pkt->flags & AV_PKT_FLAG_KEY? MONA_FRAME_KEY : MONA_FRAME_INTER;
	unsigned int compositionOffset = pkt->pts - pkt->dts;

	// 11CCCCCC FFFFF0ON [OOOOOOOO OOOOOOOO] [NNNNNNNN] TTTTTTTT TTTTTTTT TTTTTTTT TTTTTTTT
	/// C = codec
	/// F = frame (0-15)
	/// O = composition offset
	/// N = track
	/// T = time
	avio_w8(pb, (MONA_TYPE_VIDEO << 6) | (ff_codec_get_tag(mona_video_codec_ids, par->codec_id) & 0x3F));
	avio_w8(pb, (frame << 3) | (compositionOffset ? 2 : 0) | (pkt->stream_index != 1 ? 1 : 0)); // TODO: 0 or 1?
	if (compositionOffset)
		avio_wb16(pb, compositionOffset);
	if (pkt->stream_index != 1) // TODO: 0 or 1?
		avio_w8(pb, pkt->stream_index);
	avio_wb32(pb, pkt->dts); // in last to be removed easly if protocol has already time info in its protocol header
}

static void mona_write_audio_header(AVFormatContext *s, AVPacket *pkt) {
	AVIOContext *pb = s->pb;
	AVCodecParameters *par = s->streams[pkt->stream_index]->codecpar;
	unsigned short value, index;
	typedef struct Rate {
		unsigned int rate;
		unsigned int index;
	} Rate;
	static const Rate Rates[]={
		{ 0, 0 },
		{ 5512, 1 },
		{ 7350, 2 },
		{ 8000, 3 },
		{ 11025, 4 },
		{ 12000, 5 },
		{ 16000, 6 },
		{ 18900, 7 },
		{ 22050, 8 },
		{ 24000, 9 },
		{ 32000, 10 },
		{ 37800, 11 },
		{ 44056, 12 },
		{ 44100, 13 },
		{ 47250, 14 },
		{ 48000, 15 },
		{ 50000, 16 },
		{ 50400, 17 },
		{ 64000, 18 },
		{ 88200, 19 },
		{ 96000, 20 },
		{ 176400, 21 },
		{ 192000, 22 },
		{ 352800, 23 },
		{ 2822400, 24 },
		{ 5644800, 25 }
	};

	// 10CCCCCC SSSSSSSS RRRRR0IN [NNNNNNNN] TTTTTTTT TTTTTTTT TTTTTTTT TTTTTTTT
	/// C = codec
	/// R = rate "index"
	/// S = channels
	/// I = is config
	/// N = track
	/// T = time
	avio_w8(pb, (MONA_TYPE_AUDIO << 6) | (ff_codec_get_tag(mona_audio_codec_ids, par->codec_id) & 0x3F));
	avio_w8(pb, par->channels);

	for (index = 0; index < sizeof(Rates); ++index) {
		if (par->sample_rate == Rates[index].rate) {
			value = Rates[index].index << 3;
			break;
		}
	}
	if (index == sizeof(Rates)) {
		// if unsupported, set to 0 (to try to use config packet on player side)
		value = 0;
		av_log(s, AV_LOG_WARNING, "Rate %d non supported by Mona audio format", par->sample_rate);
	}

	if (pkt->flags & AV_PKT_FLAG_KEY)
		value |= 2;
	if (pkt->stream_index == 1) // TODO: 0 or 1?
		avio_w8(pb, value);
	else {
		avio_w8(pb, value & 1);
		avio_w8(pb, pkt->stream_index);
	}
	avio_wb32(pb, pkt->dts); // in last to be removed easly if protocol has already time info in its protocol header
}

static int mona_write_packet(AVFormatContext *s, AVPacket *pkt)
{
    AVIOContext *pb      = s->pb;
    AVCodecParameters *par = s->streams[pkt->stream_index]->codecpar;
    MonaContext *flv      = s->priv_data;
	//FLVStreamContext *sc = s->streams[pkt->stream_index]->priv_data;
	unsigned ts;
	int size = pkt->size;
	uint8_t *data = NULL;
	int flags = -1, flags_size, ret;
	//int64_t cur_offset = avio_tell(pb);

	switch (par->codec_type) {
	case AVMEDIA_TYPE_VIDEO:
		mona_write_video_header(s, pkt);
		break;
	case AVMEDIA_TYPE_AUDIO:
		mona_write_audio_header(s, pkt);
		break;
	case AVMEDIA_TYPE_SUBTITLE:
	case AVMEDIA_TYPE_DATA:
		
		break;
	default:
		return AVERROR(EINVAL);
	}

	/*if (par->codec_id == AV_CODEC_ID_H264 || par->codec_id == AV_CODEC_ID_MPEG4) {
		/* check if extradata looks like mp4 formatted *
		if (par->extradata_size > 0 && *(uint8_t*)par->extradata != 1)
			if ((ret = ff_avc_parse_nal_units_buf(pkt->data, &data, &size)) < 0)
				return ret;
	}*/

	avio_write(pb, data? data : pkt->data, size);

	av_free(data);

	return pb->error;
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
