package com.feyconsuelo.domain.usecase.videocategory;

import com.feyconsuelo.domain.model.videocategory.VideoCategoryRequest;

public interface UpdateVideoCategory {

    void execute(Long videoCategoryId, VideoCategoryRequest videoCategoryRequest);

}
