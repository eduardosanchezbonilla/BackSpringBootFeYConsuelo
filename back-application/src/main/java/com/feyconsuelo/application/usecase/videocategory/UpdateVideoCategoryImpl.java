package com.feyconsuelo.application.usecase.videocategory;

import com.feyconsuelo.application.service.videocategory.VideoCategoryService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.videocategory.VideoCategoryRequest;
import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;
import com.feyconsuelo.domain.usecase.videocategory.UpdateVideoCategory;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UpdateVideoCategoryImpl implements UpdateVideoCategory {

    private final VideoCategoryService videoCategoryService;
    private final ResizeImageImpl resizeImageService;

    @Value("${default-images.video-category}")
    private String defaultVideoCategoryImage;

    @Override
    public void execute(final Long videoCategoryId, final VideoCategoryRequest videoCategoryRequest) {

        final Optional<VideoCategoryResponse> videoCategoryThumbnailImage = this.videoCategoryService.get(videoCategoryId, true);
        final Optional<VideoCategoryResponse> videoCategoryOriginalImage = this.videoCategoryService.get(videoCategoryId, false);

        if (videoCategoryThumbnailImage.isEmpty() || videoCategoryOriginalImage.isEmpty()) {
            throw new NotFoundException("No existe la categoria que desea actualizar");
        }

        // si la categoria que viene es igual que e thumbnail, no la guardamos
        // videoCategoryRequest.getImage(), trae el thumbnail (pq es el que devolvimos en el listado)
        // videoCategoryThumbnailImage.getImage(), tiene la imagen thumbnail (pq hemnos pasado true)
        if (videoCategoryRequest.getImage().equals(videoCategoryThumbnailImage.get().getImage())) {
            videoCategoryRequest.setImage(videoCategoryOriginalImage.get().getImage());
        }

        // si estan enviando imagen, debemos redimensionarla
        if (StringUtils.isNotEmpty(videoCategoryRequest.getImage()) && !videoCategoryRequest.getImage().equals(this.defaultVideoCategoryImage)) {
            videoCategoryRequest.setImageThumbnail(this.resizeImageService.resizeImage(videoCategoryRequest.getImage()));
        }

        this.videoCategoryService.update(videoCategoryId, videoCategoryRequest);
    }

}
