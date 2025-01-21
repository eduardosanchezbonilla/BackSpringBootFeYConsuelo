package com.feyconsuelo.apirest.converter.video;

import com.feyconsuelo.domain.model.video.VideoGroupByCategoryResponse;
import com.feyconsuelo.openapi.model.VideoGroupByCategoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.time.LocalDateTime;
import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoGroupByCategoryListResponseToVideoGroupByCategoryListResponseDtoConverter {

    private final VideoGroupByCategoryResponseToVideoGroupByCategoryResponseDtoConverter videoGroupByCategoryResponseToVideoGroupByCategoryResponseDtoConverter;

    public List<VideoGroupByCategoryResponseDto> convert(final List<VideoGroupByCategoryResponse> videoGroupByCategoryResponseList) {
        if (CollectionUtils.isEmpty(videoGroupByCategoryResponseList)) {
            return List.of();
        }
        return videoGroupByCategoryResponseList.stream()
                .sorted(Comparator.<VideoGroupByCategoryResponse, LocalDateTime>comparing(
                                group -> group.getCategory().getDate()
                        ).reversed()
                )
                .map(this.videoGroupByCategoryResponseToVideoGroupByCategoryResponseDtoConverter::convert)
                .toList();
    }

}
