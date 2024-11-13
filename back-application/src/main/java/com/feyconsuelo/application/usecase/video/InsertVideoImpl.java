package com.feyconsuelo.application.usecase.video;

import com.feyconsuelo.application.service.video.VideoService;
import com.feyconsuelo.application.usecase.videocategory.GetVideoCategoryImpl;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.model.video.VideoRequest;
import com.feyconsuelo.domain.usecase.video.InsertVideo;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertVideoImpl implements InsertVideo {

    private final VideoService videoService;

    private final GetVideoCategoryImpl getVideoCategory;

    @Override
    public void execute(final VideoRequest videoRequest) {

        // comprobamos si la voz que estan pasando existe, sino devolvemos error
        final var category = this.getVideoCategory.execute(videoRequest.getVideoCategoryId());

        if (category.isEmpty()) {
            throw new BadRequestException("La categoria introducida no existe");
        }

        // insertamos el musico
        this.videoService.insert(videoRequest);
    }

}
