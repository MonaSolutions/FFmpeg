/*
 * Mona muxer
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

#include "avio_internal.h"
#include "avio.h"
#include "avc.h"
#include "avformat.h"
#include "internal.h"
#include "libavutil/opt.h"
#include "libavcodec/aacenctab.h"
#include "libavcodec/h264.h"

#define MONA_CODEC_RAW				0

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
    { AV_CODEC_ID_MP3,        MONA_CODEC_MP3 },
    { AV_CODEC_ID_PCM_U8,     MONA_CODEC_RAW },
    { AV_CODEC_ID_PCM_S16BE,  MONA_CODEC_RAW },
    { AV_CODEC_ID_PCM_S16LE,  MONA_CODEC_PCM_LITTLE },
    { AV_CODEC_ID_ADPCM_SWF,  MONA_CODEC_ADPCM },
    { AV_CODEC_ID_AAC,        MONA_CODEC_AAC },
    { AV_CODEC_ID_NELLYMOSER, MONA_CODEC_NELLYMOSER },
    { AV_CODEC_ID_PCM_MULAW,  MONA_CODEC_G711U },
    { AV_CODEC_ID_PCM_ALAW,   MONA_CODEC_G711A },
    { AV_CODEC_ID_SPEEX,      MONA_CODEC_SPEEX },
    { AV_CODEC_ID_NONE,       0 }
};

typedef struct MonaContext {
    AVClass *av_class;
    
} MonaContext;

static int unsupported_codec(AVFormatContext *s,
	const char* type, int codec_id)
{
	const AVCodecDescriptor *desc = avcodec_descriptor_get(codec_id);
	av_log(s, AV_LOG_ERROR,
		"%s codec %s not compatible with Mona\n",
		type,
		desc ? desc->name : "unknown");
	return AVERROR(ENOSYS);
}

static int mona_init(struct AVFormatContext *s)
{
    int i;

	for (i = 0; i < s->nb_streams; i++) {
		AVCodecParameters *par = s->streams[i]->codecpar;
		switch (par->codec_type) {
		case AVMEDIA_TYPE_VIDEO:
			if (!ff_codec_get_tag(mona_video_codec_ids, par->codec_id))
				return unsupported_codec(s, "Video", par->codec_id);
			break;
		case AVMEDIA_TYPE_AUDIO:
			if (!ff_codec_get_tag(mona_audio_codec_ids, par->codec_id))
				return unsupported_codec(s, "Audio", par->codec_id);
			break;
		case AVMEDIA_TYPE_DATA:
			/*if (par->codec_id != AV_CODEC_ID_TEXT && par->codec_id != AV_CODEC_ID_NONE)
				return unsupported_codec(s, "Data", par->codec_id);*/
			break;
		case AVMEDIA_TYPE_SUBTITLE:
			/*if (par->codec_id != AV_CODEC_ID_TEXT) {
				av_log(s, AV_LOG_ERROR, "Subtitle codec '%s' for stream %d is not compatible with Mona\n",
					avcodec_get_name(par->codec_id), i);
				return AVERROR_INVALIDDATA;
			}*/
			break;
		default:
			av_log(s, AV_LOG_ERROR, "Codec type '%s' for stream %d is not compatible with Mona\n",
				av_get_media_type_string(par->codec_type), i);
			return AVERROR(EINVAL);
		}
		avpriv_set_pts_info(s->streams[i], 32, 1, 1000); /* 32 bit pts in ms */
	}

    return 0;
}

/* Rewriting of ff_isom_write_avcc with the following corrections :
 - write NALU size on 4 bytes
 - remove 5 bytes before SPS and PPS
 - remove SPS and PPS size
*/
static int mona_write_avcc(AVIOContext *pb, const uint8_t *data, int len)
{
	AVIOContext *sps_pb = NULL, *pps_pb = NULL;
	uint8_t *buf = NULL, *end, *start = NULL;
	uint8_t *sps = NULL, *pps = NULL;
	uint32_t sps_size = 0, pps_size = 0;
	int ret, nb_sps = 0, nb_pps = 0;

	if (len <= 6)
		return AVERROR_INVALIDDATA;

	/* check for H.264 start code */
	if (AV_RB32(data) != 0x00000001 &&
		AV_RB24(data) != 0x000001) {
		avio_write(pb, data, len);
		return 0;
	}

	ret = ff_avc_parse_nal_units_buf(data, &buf, &len);
	if (ret < 0)
		return ret;
	start = buf;
	end = buf + len;

	ret = avio_open_dyn_buf(&sps_pb);
	if (ret < 0)
		goto fail;
	ret = avio_open_dyn_buf(&pps_pb);
	if (ret < 0)
		goto fail;

	/* look for sps and pps */
	while (end - buf > 4) {
		uint32_t size;
		uint8_t nal_type;
		size = FFMIN(AV_RB32(buf), end - buf - 4);
		buf += 4;
		nal_type = buf[0] & 0x1f;

		if (nal_type == 7) { /* SPS */
			nb_sps++;
			if (size > UINT16_MAX || nb_sps >= H264_MAX_SPS_COUNT) {
				ret = AVERROR_INVALIDDATA;
				goto fail;
			}
			avio_wb32(sps_pb, size);
			avio_write(sps_pb, buf, size);
		}
		else if (nal_type == 8) { /* PPS */
			nb_pps++;
			if (size > UINT16_MAX || nb_pps >= H264_MAX_PPS_COUNT) {
				ret = AVERROR_INVALIDDATA;
				goto fail;
			}
			avio_wb32(pps_pb, size);
			avio_write(pps_pb, buf, size);
		}

		buf += size;
	}
	sps_size = avio_close_dyn_buf(sps_pb, &sps);
	pps_size = avio_close_dyn_buf(pps_pb, &pps);

	if (sps_size < 6 || !pps_size) {
		ret = AVERROR_INVALIDDATA;
		goto fail;
	}

	//avio_w8(pb, 0xe0 | nb_sps); /* 3 bits reserved (111) + 5 bits number of sps */
	avio_write(pb, sps, sps_size);
	//avio_w8(pb, nb_pps); /* number of pps */
	avio_write(pb, pps, pps_size);

fail:
	if (!sps)
		avio_close_dyn_buf(sps_pb, &sps);
	if (!pps)
		avio_close_dyn_buf(pps_pb, &pps);
	av_free(sps);
	av_free(pps);
	av_free(start);

	return ret;
}

static void mona_write_video_header(AVFormatContext *s, int streamindex, unsigned int frame, int size, int64_t pts, int64_t dts) {
	AVIOContext *pb = s->pb;
	AVCodecParameters *par = s->streams[streamindex]->codecpar;
	unsigned int compositionOffset = pts - dts;
	streamindex = 1; // TODO fix this

	avio_wb32(pb, (compositionOffset ? (streamindex == 1 ? 8 : 9) : (streamindex == 1 ? 6 : 7)) + size);

	// 11CCCCCC FFFFF0ON [OOOOOOOO OOOOOOOO] [NNNNNNNN] TTTTTTTT TTTTTTTT TTTTTTTT TTTTTTTT
	/// C = codec
	/// F = frame (0-15)
	/// O = composition offset
	/// N = track
	/// T = time
	avio_w8(pb, (MONA_TYPE_VIDEO << 6) | (ff_codec_get_tag(mona_video_codec_ids, par->codec_id) & 0x3F));
	avio_w8(pb, (frame << 3) | (compositionOffset ? 2 : 0) | (streamindex != 1 ? 1 : 0));
	if (compositionOffset)
		avio_wb16(pb, compositionOffset);
	if (streamindex != 1)
		avio_w8(pb, streamindex);
	avio_wb32(pb, dts); // in last to be removed easly if protocol has already time info in its protocol header
}

static void mona_write_audio_header(AVFormatContext *s, int streamindex, short isConfig, int size, int64_t ts) {
	AVIOContext *pb = s->pb;
	AVCodecParameters *par = s->streams[streamindex]->codecpar;
	unsigned short value, index;
	typedef struct Rate {
		unsigned int rate;
		unsigned int index;
	} Rate;
	static const Rate Rates[] = {
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
	streamindex = 1; // TODO fix this
	avio_wb32(pb, (streamindex == 1 ? 7 : 8) + size);

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
		av_log(s, AV_LOG_WARNING, "Rate %d non supported by Mona audio format\n", par->sample_rate);
	}

	if (isConfig)
		value |= 2;
	if (streamindex == 1)
		avio_w8(pb, value);
	else {
		avio_w8(pb, value & 1);
		avio_w8(pb, streamindex);
	}
	avio_wb32(pb, ts); // in last to be removed easly if protocol has already time info in its protocol header
}

static void mona_write_codec_config(AVFormatContext* s, int streamindex, AVCodecParameters* par, uint64_t dts) {
	int64_t data_size;
	AVIOContext *pb = s->pb;

	if (par->codec_id != AV_CODEC_ID_AAC && par->codec_id != AV_CODEC_ID_H264
		&& par->codec_id != AV_CODEC_ID_MPEG4)
		return;

	if (par->codec_id == AV_CODEC_ID_AAC) {
		
		mona_write_audio_header(s, streamindex, 1, par->extradata_size ? par->extradata_size : 2, dts);
		// TODO: it's strange that without this patch the stream goes bad on player side, Mona issue?
		if (!par->extradata_size) {
			int rateIndex;
			int channels = par->channels - (par->channels == 8 ? 1 : 0);
			for (rateIndex = 0; rateIndex < 16;
				rateIndex++)
				if (par->sample_rate
					== mpeg4audio_sample_rates[rateIndex])
					break;

			avio_w8(pb, ((par->profile + 1) << 3) | ((rateIndex & 0x0F) >> 1)); // profile + 3 first bits of rateIndex
			avio_w8(pb, ((rateIndex & 0x01) << 7) | ((channels & 0x0F) << 3)); // last bits of rateIndex + channels
		}
		avio_write(pb, par->extradata, par->extradata_size);
	}
	else {
		int pos = avio_tell(pb);
		mona_write_video_header(s, streamindex, MONA_FRAME_CONFIG, 0, dts, dts);

		// SPS + PPS
		mona_write_avcc(pb, par->extradata, par->extradata_size);

		// patch size
		data_size = avio_tell(pb) - pos;
		avio_seek(pb, -data_size, SEEK_CUR);
		avio_wb32(pb, data_size - 4);
		avio_skip(pb, data_size - 4);
	}
}

static int mona_write_header(AVFormatContext *s)
{
	int i;

	for (i = 0; i < s->nb_streams; i++)
		mona_write_codec_config(s, i, s->streams[i]->codecpar, 0);
   
    return 0;
}

/*static int mona_write_trailer(AVFormatContext *s)
{
    AVIOContext *pb = s->pb;
    MonaContext *wg = s->priv_data;
	av_log(s, AV_LOG_INFO, "mona_write_trailer\n");

    return 0;
}*/

static int mona_write_packet(AVFormatContext *s, AVPacket *pkt)
{
    AVIOContext *pb      = s->pb;
    AVCodecParameters *par = s->streams[pkt->stream_index]->codecpar;
	int size = pkt->size;
	uint8_t *data = NULL;
	int ret;

	if (par->codec_id == AV_CODEC_ID_AAC || par->codec_id == AV_CODEC_ID_H264
		|| par->codec_id == AV_CODEC_ID_MPEG4) {
		int side_size = 0;
		uint8_t *side = av_packet_get_side_data(pkt, AV_PKT_DATA_NEW_EXTRADATA, &side_size);
		if (side && side_size > 0 && (side_size != par->extradata_size || memcmp(side, par->extradata, side_size))) {
			av_free(par->extradata);
			par->extradata = av_mallocz(side_size + AV_INPUT_BUFFER_PADDING_SIZE);
			if (!par->extradata) {
				par->extradata_size = 0;
				return AVERROR(ENOMEM);
			}
			memcpy(par->extradata, side, side_size);
			par->extradata_size = side_size;
			mona_write_codec_config(s, pkt->stream_index, par, pkt->dts);
		}
	}

	if (par->codec_id == AV_CODEC_ID_H264 || par->codec_id == AV_CODEC_ID_MPEG4) {
		/* check if extradata looks like mp4 formatted */
		if (par->extradata_size > 0 && *(uint8_t*)par->extradata != 1)
			if ((ret = ff_avc_parse_nal_units_buf(pkt->data, &data, &size)) < 0) {
				av_log(s, AV_LOG_WARNING, "error during nal parse %d\n", ret);
				return ret;
			}
	}

	switch (par->codec_type) {
	case AVMEDIA_TYPE_VIDEO:
		mona_write_video_header(s, pkt->stream_index, 
			pkt->flags & AV_PKT_FLAG_KEY ? MONA_FRAME_KEY : MONA_FRAME_INTER, 
			size, pkt->pts, pkt->dts);
		break;
	case AVMEDIA_TYPE_AUDIO:
		mona_write_audio_header(s, pkt->stream_index, 0, size, pkt->dts);
		break;
	case AVMEDIA_TYPE_SUBTITLE:
	case AVMEDIA_TYPE_DATA:
		av_log(s, AV_LOG_WARNING, "data type and subtitle are not handled for now");
		break;
	default:
		return AVERROR(EINVAL);
	}

	avio_write(pb, (data? data : pkt->data), size);

	av_free(data);

	return pb->error;
}

static int mona_check_bitstream(struct AVFormatContext *s, const AVPacket *pkt)
{
    int ret = 1;
	AVStream *st = s->streams[pkt->stream_index];

	if (st->codecpar->codec_id == AV_CODEC_ID_AAC) {
		if (pkt->size > 2 && (AV_RB16(pkt->data) & 0xfff0) == 0xfff0)
			ret = ff_stream_add_bitstream_filter(st, "aac_adtstoasc", NULL);
	}
    
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
    .audio_codec    = CONFIG_LIBMP3LAME ? AV_CODEC_ID_MP3 : MONA_CODEC_AAC,
    .video_codec    = AV_CODEC_ID_H264,
    .init           = mona_init,
    .write_header   = mona_write_header,
    .write_packet   = mona_write_packet,
    //.write_trailer  = mona_write_trailer,
    .check_bitstream= mona_check_bitstream,
    .codec_tag      = (const AVCodecTag* const []) {
                          mona_video_codec_ids, mona_audio_codec_ids, 0
                      },
    .flags          = AVFMT_GLOBALHEADER | AVFMT_VARIABLE_FPS |
                      AVFMT_TS_NONSTRICT,
    .priv_class     = &mona_muxer_class,
};
