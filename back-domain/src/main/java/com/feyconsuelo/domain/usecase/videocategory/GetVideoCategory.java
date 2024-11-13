package com.feyconsuelo.domain.usecase.videocategory;

import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;

import java.util.Optional;

public interface GetVideoCategory {

    Optional<VideoCategoryResponse> execute(Long videoCategoryId);

}
