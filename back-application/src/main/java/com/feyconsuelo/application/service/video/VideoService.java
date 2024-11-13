package com.feyconsuelo.application.service.video;

import com.feyconsuelo.domain.model.video.VideoRequest;
import com.feyconsuelo.domain.model.video.VideoResponse;

import java.util.List;
import java.util.Optional;

public interface VideoService {

    List<VideoResponse> getAll();

    Optional<VideoResponse> get(Long videoId);

    List<VideoResponse> getByCategory(Long categoryId);

    void insert(VideoRequest videoRequest);

    void update(Long videoId, VideoRequest videoRequest);

    void delete(Long videoId);

    void logicalDelete(Long videoId);
}
