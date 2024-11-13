package com.feyconsuelo.infrastructure.converter.video;

import com.feyconsuelo.domain.model.video.VideoResponse;
import com.feyconsuelo.infrastructure.entities.video.VideoEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoEntityListToVideoResponseListConverter {

    private final VideoEntityToVideoResponseConverter videoEntityToVideoResponseConverter;

    public List<VideoResponse> convert(final List<VideoEntity> videoEntityList) {
        if (CollectionUtils.isEmpty(videoEntityList)) {
            return List.of();
        }
        return videoEntityList.stream()
                .map(this.videoEntityToVideoResponseConverter::convert)
                .toList();
    }
}
