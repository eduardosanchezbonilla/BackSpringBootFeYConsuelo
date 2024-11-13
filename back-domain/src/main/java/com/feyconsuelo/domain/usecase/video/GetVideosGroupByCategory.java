package com.feyconsuelo.domain.usecase.video;

import com.feyconsuelo.domain.model.video.VideoGroupByCategoryRequest;
import com.feyconsuelo.domain.model.video.VideoGroupByCategoryResponse;

import java.util.List;

public interface GetVideosGroupByCategory {

    List<VideoGroupByCategoryResponse> execute(final VideoGroupByCategoryRequest videoGroupByCategoryRequest);

}
