package com.feyconsuelo.application.usecase.videocategory;

import com.feyconsuelo.application.service.videocategory.VideoCategoryService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.model.videocategory.VideoCategoryRequest;
import com.feyconsuelo.domain.usecase.videocategory.UpdateVideoCategory;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdateVideoCategoryImpl implements UpdateVideoCategory {

    private final VideoCategoryService videoCategoryService;
    private final ResizeImageImpl resizeImageService;

    @Value("${default-images.video-category}")
    private String defaultVideoCategoryImage;

    @Override
    public void execute(final Long videoCategoryId, final VideoCategoryRequest videoCategoryRequest) {
        // si estan enviando imagen, debemos redimensionarla
        if (StringUtils.isNotEmpty(videoCategoryRequest.getImage()) && !videoCategoryRequest.getImage().equals(this.defaultVideoCategoryImage)) {
            videoCategoryRequest.setImage(this.resizeImageService.resizeImage(videoCategoryRequest.getImage()));
        }

        this.videoCategoryService.update(videoCategoryId, videoCategoryRequest);
    }

}
