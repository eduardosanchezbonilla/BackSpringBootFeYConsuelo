package com.feyconsuelo.apirest.converter.videocategory;

import com.feyconsuelo.application.service.utils.DateService;
import com.feyconsuelo.domain.model.videocategory.VideoCategoryRequest;
import com.feyconsuelo.openapi.model.VideoCategoryRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoCategoryRequestDtoToVideoCategoryRequestConverter {

    private final DateService dateService;

    public VideoCategoryRequest convert(final VideoCategoryRequestDto videoCategoryRequestDto) {
        return VideoCategoryRequest.builder()
                .name(videoCategoryRequestDto.getName())
                .isPublic(videoCategoryRequestDto.getIsPublic())
                .order(videoCategoryRequestDto.getOrder())
                .image(videoCategoryRequestDto.getImage())
                .date(videoCategoryRequestDto.getDate() == null ? LocalDateTime.now() : this.dateService.stringToDate(videoCategoryRequestDto.getDate(), DateTimeFormatter.ISO_DATE_TIME))
                .build();
    }

}
