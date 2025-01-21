package com.feyconsuelo.application.usecase.videocategory;

import com.feyconsuelo.application.service.videocategory.VideoCategoryService;
import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;
import com.feyconsuelo.domain.usecase.videocategory.GetVideoCategoryImage;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetVideoCategoryImageImpl implements GetVideoCategoryImage {

    private final VideoCategoryService videoCategoryService;

    @Override
    public Optional<VideoCategoryResponse> execute(final Long videoCategoryId) {
        return this.videoCategoryService.get(videoCategoryId, Boolean.FALSE);
    }
}
