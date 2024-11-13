package com.feyconsuelo.domain.usecase.videocategory;

import com.feyconsuelo.domain.model.videocategory.VideoCategoryRequest;

public interface InsertVideoCategory {

    void execute(VideoCategoryRequest videoCategoryRequest);

}
