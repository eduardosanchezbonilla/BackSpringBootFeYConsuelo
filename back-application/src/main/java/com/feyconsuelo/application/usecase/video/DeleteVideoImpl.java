package com.feyconsuelo.application.usecase.video;

import com.feyconsuelo.application.service.video.VideoService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.video.VideoResponse;
import com.feyconsuelo.domain.usecase.video.DeleteVideo;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class DeleteVideoImpl implements DeleteVideo {

    private final VideoService videoService;

    @Override
    public void execute(final Long videoId) {
        final Optional<VideoResponse> videoResponse = this.videoService.get(videoId);

        if (videoResponse.isEmpty()) {
            throw new NotFoundException("No existe ningún video con el ID introducido");
        } else {
            this.videoService.logicalDelete(videoId);
        }
    }

}
