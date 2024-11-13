package com.feyconsuelo.application.usecase.video;

import com.feyconsuelo.application.service.video.VideoService;
import com.feyconsuelo.application.service.videocategory.VideoCategoryService;
import com.feyconsuelo.domain.model.video.VideoGroupByCategoryRequest;
import com.feyconsuelo.domain.model.video.VideoGroupByCategoryResponse;
import com.feyconsuelo.domain.model.video.VideoResponse;
import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;
import com.feyconsuelo.domain.usecase.video.GetVideosGroupByCategory;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetVideosGroupByCategoryImpl implements GetVideosGroupByCategory {

    private final VideoCategoryService videoCategoryService;
    private final VideoService videoService;

    private Boolean filterVideo(final VideoResponse video, final VideoGroupByCategoryRequest videoGroupByCategoryRequest) {
        if (StringUtils.isEmpty(videoGroupByCategoryRequest.getName())) {
            return Boolean.TRUE;
        } else {
            return video.getName().toUpperCase().contains(videoGroupByCategoryRequest.getName().toUpperCase());
        }
    }

    private List<VideoResponse> filterVideos(final List<VideoResponse> videos,
                                             final VideoGroupByCategoryRequest videoGroupByCategoryRequest) {

        return Boolean.TRUE.equals(CollectionUtils.isEmpty(videos)) ?
                videos :
                videos.stream()
                        .filter(video -> this.filterVideo(video, videoGroupByCategoryRequest))
                        .toList();
    }

    @Override
    public List<VideoGroupByCategoryResponse> execute(final VideoGroupByCategoryRequest videoGroupByCategoryRequest) {

        // obtenemos todas las categorias
        final List<VideoCategoryResponse> categories = this.videoCategoryService.getAll();

        // obtenemos todos los videos
        final List<VideoResponse> videos = this.videoService.getAll();

        final List<VideoResponse> filterVideos = this.filterVideos(videos, videoGroupByCategoryRequest);

        // recorremos todas las voces y en cada una de ellos metemos los musicos que coincidan en voz
        return categories.stream()
                .map(
                        category -> VideoGroupByCategoryResponse.builder()
                                .category(category)
                                .videos(
                                        filterVideos.stream()
                                                .filter(video -> video.getVideoCategory().getId().equals(category.getId()))
                                                .toList()
                                )
                                .build()
                )
                //.filter(musicianGroupByVoiceResponse -> Boolean.FALSE.equals(musicianGroupByVoiceResponse.getMusicians().isEmpty()))
                .toList();
    }
}
