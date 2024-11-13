package com.feyconsuelo.apirest.converter.video;

import com.feyconsuelo.domain.model.video.VideoResponse;
import com.feyconsuelo.openapi.model.VideoResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoResponseListToVideoResponseDtoListConverter {

    private final VideoResponseToVideoResponseDtoConverter videoResponseToVideoResponseDtoConverter;

    public List<VideoResponseDto> convert(final List<VideoResponse> videoResponseList) {
        if (CollectionUtils.isEmpty(videoResponseList)) {
            return List.of();
        }
        return videoResponseList.stream()
                .map(this.videoResponseToVideoResponseDtoConverter::convert)
                .sorted(Comparator.comparing(VideoResponseDto::getOrder))
                .toList();
    }

}
