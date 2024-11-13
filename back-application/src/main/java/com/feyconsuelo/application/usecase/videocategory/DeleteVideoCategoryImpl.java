package com.feyconsuelo.application.usecase.videocategory;

import com.feyconsuelo.application.service.video.VideoService;
import com.feyconsuelo.application.service.videocategory.VideoCategoryService;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.model.video.VideoResponse;
import com.feyconsuelo.domain.usecase.videocategory.DeleteVideoCategory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Component
@RequiredArgsConstructor
public class DeleteVideoCategoryImpl implements DeleteVideoCategory {

    private final VideoCategoryService videoCategoryService;
    private final VideoService videoService;


    @Override
    public void execute(final Long videoCategoryId) {
        // solo permitiremos borrar categorias que no esten asociadas a ningun video
        final List<VideoResponse> videos = this.videoService.getByCategory(videoCategoryId);
        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(videos))) {
            throw new BadRequestException("No se pueden eliminar categorias asociadas a videos");
        }
        this.videoCategoryService.logicalDelete(videoCategoryId);
    }

}
