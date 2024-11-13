package com.feyconsuelo.infrastructure.converter.videocategory;

import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;
import com.feyconsuelo.infrastructure.entities.videocategory.VideoCategoryEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoCategoryEntityToVideoCategoryResponseConverter {

    public VideoCategoryResponse convert(final VideoCategoryEntity videoCategoryEntity) {
        return VideoCategoryResponse.builder()
                .id(videoCategoryEntity.getId())
                .name(videoCategoryEntity.getName())
                .isPublic(videoCategoryEntity.getIsPublic())
                .order(videoCategoryEntity.getOrder())
                .image(videoCategoryEntity.getImage())
                .deleteDate(videoCategoryEntity.getDeleteDate())
                .build();
    }
    
}
