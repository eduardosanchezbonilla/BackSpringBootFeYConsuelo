package com.feyconsuelo.application.usecase.video;

import com.feyconsuelo.application.service.video.VideoService;
import com.feyconsuelo.application.usecase.videocategory.GetVideoCategoryImpl;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.video.VideoRequest;
import com.feyconsuelo.domain.model.video.VideoResponse;
import com.feyconsuelo.domain.usecase.video.UpdateVideo;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UpdateVideoImpl implements UpdateVideo {

    private final VideoService videoService;

    private final GetVideoCategoryImpl getVideoCategory;

    @Override
    public void execute(final Long videoId, final VideoRequest videoRequest) {

        final Optional<VideoResponse> videoResponse = this.videoService.get(videoId);

        if (videoResponse.isEmpty()) {
            throw new NotFoundException("No existe el video que desea actualizar");
        }

        // comprobamos si la voz que estan pasando existe, sino devolvemos error
        final var category = this.getVideoCategory.execute(videoRequest.getVideoCategoryId());

        if (category.isEmpty()) {
            throw new BadRequestException("La categoria introducida no existe");
        }

        this.videoService.update(videoId, videoRequest);

    }

}
