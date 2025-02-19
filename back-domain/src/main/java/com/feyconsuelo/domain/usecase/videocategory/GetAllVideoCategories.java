package com.feyconsuelo.domain.usecase.videocategory;

import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;

import java.util.List;

public interface GetAllVideoCategories {

    List<VideoCategoryResponse> execute(Boolean isThumbnail);

}
