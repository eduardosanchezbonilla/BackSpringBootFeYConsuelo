package com.feyconsuelo.apirest.service.videocategory.query;

import com.feyconsuelo.apirest.converter.videocategory.VideoCategoryResponseListToVideoCategoryResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.videocategory.VideoCategoryResponseToVideoCategoryResponseDtoConverter;
import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;
import com.feyconsuelo.domain.usecase.videocategory.GetAllVideoCategories;
import com.feyconsuelo.domain.usecase.videocategory.GetVideoCategory;
import com.feyconsuelo.domain.usecase.videocategory.GetVideoCategoryImage;
import com.feyconsuelo.openapi.model.VideoCategoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetVideoCategoryService {

    private final GetAllVideoCategories getAllVideoCategories;

    private final GetVideoCategory getVideoCategory;

    private final GetVideoCategoryImage getVideoCategoryImage;

    private final VideoCategoryResponseToVideoCategoryResponseDtoConverter videoCategoryResponseToVideoCategoryResponseDtoConverter;

    private final VideoCategoryResponseListToVideoCategoryResponseDtoListConverter videoCategoryResponseListToVideoCategoryResponseDtoListConverter;

    public ResponseEntity<List<VideoCategoryResponseDto>> getAllVideoCategories() {
        final List<VideoCategoryResponse> videoCategoryResponseList = this.getAllVideoCategories.execute();
        if (CollectionUtils.isEmpty(videoCategoryResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.videoCategoryResponseListToVideoCategoryResponseDtoListConverter.convert(videoCategoryResponseList));
    }

    public ResponseEntity<VideoCategoryResponseDto> getVideoCategory(final Long videoCategoryId) {
        final Optional<VideoCategoryResponseDto> videoCategoryResponse = this.getVideoCategory.execute(videoCategoryId).map(this.videoCategoryResponseToVideoCategoryResponseDtoConverter::convert);
        return videoCategoryResponse.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

    public ResponseEntity<VideoCategoryResponseDto> getVideoCategoryImage(final Long videoCategoryId) {
        final Optional<VideoCategoryResponseDto> videoCategoryResponse = this.getVideoCategoryImage.execute(videoCategoryId).map(this.videoCategoryResponseToVideoCategoryResponseDtoConverter::convert);
        return videoCategoryResponse.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
