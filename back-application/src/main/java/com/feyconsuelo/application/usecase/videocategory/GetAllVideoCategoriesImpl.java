package com.feyconsuelo.application.usecase.videocategory;

import com.feyconsuelo.application.service.videocategory.VideoCategoryService;
import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;
import com.feyconsuelo.domain.usecase.videocategory.GetAllVideoCategories;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetAllVideoCategoriesImpl implements GetAllVideoCategories {

    private final VideoCategoryService videoCategoryService;

    @Override
    public List<VideoCategoryResponse> execute(final Boolean isThumbnail) {
        return this.videoCategoryService.getAll(isThumbnail);
    }
}
