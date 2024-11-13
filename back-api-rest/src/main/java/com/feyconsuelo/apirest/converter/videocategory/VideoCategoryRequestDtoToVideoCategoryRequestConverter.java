package com.feyconsuelo.apirest.converter.videocategory;

import com.feyconsuelo.domain.model.videocategory.VideoCategoryRequest;
import com.feyconsuelo.openapi.model.VideoCategoryRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoCategoryRequestDtoToVideoCategoryRequestConverter {

    public VideoCategoryRequest convert(final VideoCategoryRequestDto videoCategoryRequestDto) {
        return VideoCategoryRequest.builder()
                .name(videoCategoryRequestDto.getName())
                .isPublic(videoCategoryRequestDto.getIsPublic())
                .order(videoCategoryRequestDto.getOrder())
                .image(videoCategoryRequestDto.getImage())
                .build();
    }

}
