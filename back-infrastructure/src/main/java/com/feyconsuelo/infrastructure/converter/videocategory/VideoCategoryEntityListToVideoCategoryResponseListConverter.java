package com.feyconsuelo.infrastructure.converter.videocategory;

import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;
import com.feyconsuelo.infrastructure.entities.videocategory.VideoCategoryEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoCategoryEntityListToVideoCategoryResponseListConverter {

    private final VideoCategoryEntityToVideoCategoryResponseConverter videoCategoryEntityToVideoCategoryResponseConverter;

    public List<VideoCategoryResponse> convert(final List<VideoCategoryEntity> videoCategoryEntityList, final Boolean isThumbnail) {
        if (CollectionUtils.isEmpty(videoCategoryEntityList)) {
            return List.of();
        }
        return videoCategoryEntityList.stream()
                .map(category -> this.videoCategoryEntityToVideoCategoryResponseConverter.convert(category, isThumbnail))
                .toList();
    }
}
