package com.feyconsuelo.apirest.converter.video;

import com.feyconsuelo.apirest.converter.videocategory.VideoCategoryResponseToVideoCategoryResponseDtoConverter;
import com.feyconsuelo.domain.model.video.VideoGroupByCategoryResponse;
import com.feyconsuelo.openapi.model.VideoGroupByCategoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoGroupByCategoryResponseToVideoGroupByCategoryResponseDtoConverter {

    private final VideoCategoryResponseToVideoCategoryResponseDtoConverter videoCategoryResponseToVideoCategoryResponseDtoConverter;
    private final VideoResponseListToVideoResponseDtoListConverter videoResponseListToVideoResponseDtoListConverter;

    public VideoGroupByCategoryResponseDto convert(final VideoGroupByCategoryResponse videoGroupByCategoryResponse) {
        return VideoGroupByCategoryResponseDto.builder()
                .category(this.videoCategoryResponseToVideoCategoryResponseDtoConverter.convert(videoGroupByCategoryResponse.getCategory()))
                .videos(this.videoResponseListToVideoResponseDtoListConverter.convert(videoGroupByCategoryResponse.getVideos()))
                .build();
    }

}
