package com.feyconsuelo.infrastructure.converter.video;

import com.feyconsuelo.domain.model.video.VideoRequest;
import com.feyconsuelo.infrastructure.entities.video.VideoEntity;
import com.feyconsuelo.infrastructure.repository.VideoCategoryRepository;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoRequestToVideoEntityConverter {

    private final VideoCategoryRepository videoCategoryRepository;

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public VideoEntity convert(final VideoRequest videoRequest) {
        return VideoEntity.builder()
                .youtubeId(videoRequest.getYoutubeId())
                .videoCategory(this.videoCategoryRepository.findVideoCategoryActiveById(videoRequest.getVideoCategoryId()).orElse(null))
                .name(videoRequest.getName())
                .description(videoRequest.getDescription())
                .order(videoRequest.getOrder())
                .modifiedUser(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public VideoEntity updateEntity(final VideoEntity videoEntity,
                                    final VideoRequest videoRequest) {
        videoEntity.setYoutubeId(videoRequest.getYoutubeId());
        videoEntity.setVideoCategory(this.videoCategoryRepository.findVideoCategoryActiveById(videoRequest.getVideoCategoryId()).orElse(null));
        videoEntity.setName(videoRequest.getName());
        videoEntity.setDescription(videoRequest.getDescription());
        videoEntity.setOrder(videoRequest.getOrder());
        videoEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return videoEntity;
    }

    public VideoEntity deleteEntity(final VideoEntity videoEntity) {
        videoEntity.setDeleteDate(LocalDateTime.now());
        videoEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return videoEntity;
    }
}
