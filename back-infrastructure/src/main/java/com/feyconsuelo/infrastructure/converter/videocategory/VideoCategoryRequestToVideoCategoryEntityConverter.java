package com.feyconsuelo.infrastructure.converter.videocategory;

import com.feyconsuelo.domain.model.videocategory.VideoCategoryRequest;
import com.feyconsuelo.infrastructure.entities.videocategory.VideoCategoryEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoCategoryRequestToVideoCategoryEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    @Value("${default-images.video-category}")
    private String defaultVideoCategoryImage;

    private String getVideoCategoryImage(final VideoCategoryRequest videoCategoryRequest) {
        if (StringUtils.isEmpty(videoCategoryRequest.getImage())) {
            return videoCategoryRequest.getImage();
        } else {
            if (videoCategoryRequest.getImage().equals(this.defaultVideoCategoryImage)) {
                return null;
            } else {
                return videoCategoryRequest.getImage();
            }
        }
    }

    public VideoCategoryEntity convert(final VideoCategoryRequest videoCategoryRequest) {
        return VideoCategoryEntity.builder()
                .name(videoCategoryRequest.getName())
                .isPublic(videoCategoryRequest.getIsPublic())
                .order(videoCategoryRequest.getOrder())
                .image(this.getVideoCategoryImage(videoCategoryRequest))
                .modifiedUser(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public VideoCategoryEntity updateEntity(final VideoCategoryEntity videoCategoryEntity,
                                            final VideoCategoryRequest videoCategoryRequest) {
        videoCategoryEntity.setName(videoCategoryRequest.getName());
        videoCategoryEntity.setIsPublic(videoCategoryRequest.getIsPublic());
        videoCategoryEntity.setOrder(videoCategoryRequest.getOrder());
        videoCategoryEntity.setImage(this.getVideoCategoryImage(videoCategoryRequest));
        videoCategoryEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return videoCategoryEntity;
    }

    public VideoCategoryEntity deleteEntity(final VideoCategoryEntity videoCategoryEntity) {
        videoCategoryEntity.setDeleteDate(LocalDateTime.now());
        videoCategoryEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return videoCategoryEntity;
    }
}
