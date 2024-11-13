package com.feyconsuelo.apirest.converter.videocategory;

import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;
import com.feyconsuelo.openapi.model.VideoCategoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoCategoryResponseToVideoCategoryResponseDtoConverter {

    public VideoCategoryResponseDto convert(final VideoCategoryResponse videoCategoryResponse) {
        return VideoCategoryResponseDto.builder()
                .id(videoCategoryResponse.getId())
                .name(videoCategoryResponse.getName())
                .isPublic(videoCategoryResponse.getIsPublic())
                .order(videoCategoryResponse.getOrder())
                .image(videoCategoryResponse.getImage())
                .build();
    }

}
