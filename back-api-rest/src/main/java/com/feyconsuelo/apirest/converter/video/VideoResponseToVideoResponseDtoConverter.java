package com.feyconsuelo.apirest.converter.video;

import com.feyconsuelo.apirest.converter.videocategory.VideoCategoryResponseToVideoCategoryResponseDtoConverter;
import com.feyconsuelo.domain.model.video.VideoResponse;
import com.feyconsuelo.openapi.model.VideoResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoResponseToVideoResponseDtoConverter {

    private final VideoCategoryResponseToVideoCategoryResponseDtoConverter videoCategoryResponseToVideoCategoryResponseDtoConverter;

    public VideoResponseDto convert(final VideoResponse videoResponse) {
        return VideoResponseDto.builder()
                .id(videoResponse.getId())
                .youtubeId(videoResponse.getYoutubeId())
                .videoCategoryId(videoResponse.getVideoCategory().getId())
                .name(videoResponse.getName())
                .description(videoResponse.getDescription())
                .order(videoResponse.getOrder())
                .build();
    }

}
