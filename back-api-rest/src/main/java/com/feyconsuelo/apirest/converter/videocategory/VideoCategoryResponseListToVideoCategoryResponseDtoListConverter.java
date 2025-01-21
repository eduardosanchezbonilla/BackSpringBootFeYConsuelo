package com.feyconsuelo.apirest.converter.videocategory;

import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;
import com.feyconsuelo.openapi.model.VideoCategoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoCategoryResponseListToVideoCategoryResponseDtoListConverter {

    private final VideoCategoryResponseToVideoCategoryResponseDtoConverter videoCategoryResponseToVideoCategoryResponseDtoConverter;

    public List<VideoCategoryResponseDto> convert(final List<VideoCategoryResponse> videoCategoryResponseList) {
        if (CollectionUtils.isEmpty(videoCategoryResponseList)) {
            return List.of();
        }
        return videoCategoryResponseList.stream()
                .map(this.videoCategoryResponseToVideoCategoryResponseDtoConverter::convert)
                .sorted(Comparator.comparing(VideoCategoryResponseDto::getDate).reversed())
                .toList();
    }

}
