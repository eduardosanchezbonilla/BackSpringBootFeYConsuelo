package com.feyconsuelo.infrastructure.converter.video;

import com.feyconsuelo.domain.model.video.VideoResponse;
import com.feyconsuelo.infrastructure.converter.videocategory.VideoCategoryEntityToVideoCategoryResponseConverter;
import com.feyconsuelo.infrastructure.entities.video.VideoEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class VideoEntityToVideoResponseConverter {

    private final VideoCategoryEntityToVideoCategoryResponseConverter videoCategoryEntityToVideoCategoryResponseConverter;

    public VideoResponse convert(final VideoEntity videoEntity) {
        return VideoResponse.builder()
                .id(videoEntity.getId())
                .youtubeId(videoEntity.getYoutubeId())
                .videoCategory(this.videoCategoryEntityToVideoCategoryResponseConverter.convert(videoEntity.getVideoCategory(), Boolean.TRUE))
                .name(videoEntity.getName())
                .description(videoEntity.getDescription())
                .order(videoEntity.getOrder())
                .deleteDate(videoEntity.getDeleteDate())
                .build();
    }

}
