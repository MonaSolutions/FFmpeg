/*
 * Mona common header
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

#ifndef AVFORMAT_MONA_H
#define AVFORMAT_MONA_H

#define MONA_CODEC_RAW                          0

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

// Video codecs
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

// Audio codecs
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

#endif /* AVFORMAT_MONA_H */
