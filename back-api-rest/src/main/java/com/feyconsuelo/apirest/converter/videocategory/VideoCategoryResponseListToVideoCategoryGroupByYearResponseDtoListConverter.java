package com.feyconsuelo.apirest.converter.videocategory;

import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;
import com.feyconsuelo.openapi.model.VideoCategoryGroupByYearResponseDto;
import com.feyconsuelo.openapi.model.VideoCategoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoCategoryResponseListToVideoCategoryGroupByYearResponseDtoListConverter {

    private final VideoCategoryResponseToVideoCategoryResponseDtoConverter videoCategoryResponseToVideoCategoryResponseDtoConverter;

    public List<VideoCategoryGroupByYearResponseDto> convert(final List<VideoCategoryResponse> videoCategoryResponseList, final Boolean onlyPublic) {
        if (CollectionUtils.isEmpty(videoCategoryResponseList)) {
            return List.of();
        }

        // tenemos que agrupar por anio las categorias y devolverlas
        return videoCategoryResponseList.stream()
                .filter(category -> (Boolean.TRUE.equals(onlyPublic) && category.getIsPublic()) || Boolean.FALSE.equals(onlyPublic))
                .collect(Collectors.groupingBy(category -> category.getDate().getYear()))
                .entrySet().stream()
                .map(
                        entry -> VideoCategoryGroupByYearResponseDto.builder()
                                .year(entry.getKey() + "")
                                .categories(
                                        entry.getValue().stream()
                                                .map(this.videoCategoryResponseToVideoCategoryResponseDtoConverter::convert)
                                                .sorted(Comparator.comparing(VideoCategoryResponseDto::getDate).reversed())
                                                .toList()
                                )
                                .build()
                )
                .sorted(Comparator.comparing(VideoCategoryGroupByYearResponseDto::getYear).reversed())
                .toList();
    }

}
