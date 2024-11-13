package com.feyconsuelo.apirest.converter.video;

import com.feyconsuelo.domain.model.video.VideoRequest;
import com.feyconsuelo.openapi.model.VideoRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoRequestDtoToVideoRequestConverter {

    public VideoRequest convert(final VideoRequestDto videoRequestDto) {
        return VideoRequest.builder()
                .youtubeId(videoRequestDto.getYoutubeId())
                .videoCategoryId(videoRequestDto.getVideoCategoryId())
                .name(videoRequestDto.getName())
                .description(videoRequestDto.getDescription())
                .order(videoRequestDto.getOrder())
                .build();
    }

}
