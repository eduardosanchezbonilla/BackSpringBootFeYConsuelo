package com.feyconsuelo.apirest.converter.videocategory;

import com.feyconsuelo.application.service.utils.DateService;
import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;
import com.feyconsuelo.openapi.model.VideoCategoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.format.DateTimeFormatter;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoCategoryResponseToVideoCategoryResponseDtoConverter {

    private final DateService dateService;

    public VideoCategoryResponseDto convert(final VideoCategoryResponse videoCategoryResponse) {
        return VideoCategoryResponseDto.builder()
                .id(videoCategoryResponse.getId())
                .name(videoCategoryResponse.getName())
                .isPublic(videoCategoryResponse.getIsPublic())
                .order(videoCategoryResponse.getOrder())
                .image(videoCategoryResponse.getImage())
                .date(this.dateService.dateToString(videoCategoryResponse.getDate(), DateTimeFormatter.ISO_DATE_TIME))
                .build();
    }

}
