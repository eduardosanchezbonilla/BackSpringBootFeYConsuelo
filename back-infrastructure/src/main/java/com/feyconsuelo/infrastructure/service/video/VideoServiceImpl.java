package com.feyconsuelo.infrastructure.service.video;

import com.feyconsuelo.application.service.video.VideoService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.video.VideoRequest;
import com.feyconsuelo.domain.model.video.VideoResponse;
import com.feyconsuelo.infrastructure.converter.video.VideoEntityListToVideoResponseListConverter;
import com.feyconsuelo.infrastructure.converter.video.VideoEntityToVideoResponseConverter;
import com.feyconsuelo.infrastructure.converter.video.VideoRequestToVideoEntityConverter;
import com.feyconsuelo.infrastructure.entities.video.VideoEntity;
import com.feyconsuelo.infrastructure.repository.VideoRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class VideoServiceImpl implements VideoService {

    private final VideoRepository videoRepository;
    private final VideoRequestToVideoEntityConverter videoRequestToVideoEntityConverter;
    private final VideoEntityListToVideoResponseListConverter videoEntityListToVideoResponseListConverter;
    private final VideoEntityToVideoResponseConverter videoEntityToVideoResponseConverter;

    @Override
    public void delete(final Long videoId) {
        this.videoRepository.deleteById(videoId);
    }

    @Override
    public void logicalDelete(final Long videoId) {

        final var video = this.videoRepository.findVideoActiveById(videoId);

        if (video.isEmpty()) {
            throw new NotFoundException("No existe el video que desea eliminar");
        }

        this.videoRepository.save(this.videoRequestToVideoEntityConverter.deleteEntity(video.get()));
    }

    @Override
    public List<VideoResponse> getAll() {
        final List<VideoEntity> videos = this.videoRepository.findAllActives();
        return this.videoEntityListToVideoResponseListConverter.convert(videos);
    }

    @Override
    public List<VideoResponse> getByCategory(final Long categoryId) {
        final var videos = this.videoRepository.findVideoActiveByCategory(categoryId);
        return this.videoEntityListToVideoResponseListConverter.convert(videos);
    }

    @Override
    public void insert(final VideoRequest videoRequest) {
        this.videoRepository.save(
                this.videoRequestToVideoEntityConverter.convert(videoRequest)
        );
    }

    @Override
    public void update(final Long videoId,
                       final VideoRequest videoRequest) {

        final var video = this.videoRepository.findVideoActiveById(videoId);

        if (video.isEmpty()) {
            throw new NotFoundException("No existe el video que desea modificar");
        }

        this.videoRepository.save(
                this.videoRequestToVideoEntityConverter.updateEntity(video.get(), videoRequest)
        );
    }

    @Override
    public Optional<VideoResponse> get(final Long videoId) {
        final var video = this.videoRepository.findVideoActiveById(videoId);
        return video.map(this.videoEntityToVideoResponseConverter::convert);
    }

}
