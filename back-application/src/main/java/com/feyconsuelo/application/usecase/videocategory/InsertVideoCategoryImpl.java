package com.feyconsuelo.application.usecase.videocategory;

import com.feyconsuelo.application.service.videocategory.VideoCategoryService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.model.videocategory.VideoCategoryRequest;
import com.feyconsuelo.domain.usecase.videocategory.InsertVideoCategory;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertVideoCategoryImpl implements InsertVideoCategory {

    private final VideoCategoryService videoCategoryService;

    private final ResizeImageImpl resizeImageService;

    @Value("${default-images.video-category}")
    private String defaultVideoCategoryImage;

    @Override
    public void execute(final VideoCategoryRequest videoCategoryRequest) {

        // si estan enviando imagen, debemos redimensionarla
        if (StringUtils.isNotEmpty(videoCategoryRequest.getImage()) && !videoCategoryRequest.getImage().equals(this.defaultVideoCategoryImage)) {
            videoCategoryRequest.setImageThumbnail(this.resizeImageService.resizeImage(videoCategoryRequest.getImage()));
        }

        this.videoCategoryService.insert(videoCategoryRequest);
    }

}
