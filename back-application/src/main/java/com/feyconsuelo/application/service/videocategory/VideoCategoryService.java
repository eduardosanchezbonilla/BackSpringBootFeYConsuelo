package com.feyconsuelo.application.service.videocategory;

import com.feyconsuelo.domain.model.videocategory.VideoCategoryRequest;
import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;

import java.util.List;
import java.util.Optional;

public interface VideoCategoryService {

    void delete(Long videoCategoryId);

    void logicalDelete(Long videoCategoryId);

    List<VideoCategoryResponse> getAll();

    Optional<VideoCategoryResponse> get(Long videoCategoryId, Boolean isThumbnail);

    void insert(VideoCategoryRequest videoCategoryRequest);

    void update(Long videoCategoryId, VideoCategoryRequest videoCategoryRequest);

}
