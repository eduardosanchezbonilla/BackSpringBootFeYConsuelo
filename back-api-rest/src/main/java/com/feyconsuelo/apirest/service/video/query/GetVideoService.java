package com.feyconsuelo.apirest.service.video.query;

import com.feyconsuelo.apirest.converter.video.VideoGroupByCategoryListResponseToVideoGroupByCategoryListResponseDtoConverter;
import com.feyconsuelo.domain.model.video.VideoGroupByCategoryRequest;
import com.feyconsuelo.domain.model.video.VideoGroupByCategoryResponse;
import com.feyconsuelo.domain.usecase.video.GetVideosGroupByCategory;
import com.feyconsuelo.openapi.model.VideoGroupByCategoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetVideoService {

    private final GetVideosGroupByCategory getVideosGroupByCategory;

    private final VideoGroupByCategoryListResponseToVideoGroupByCategoryListResponseDtoConverter videoGroupByCategoryListResponseToVideoGroupByCategoryListResponseDtoConverter;

    public ResponseEntity<List<VideoGroupByCategoryResponseDto>> getVideoGroupByCategory(final VideoGroupByCategoryRequest videoGroupByCategoryRequest) {
        final List<VideoGroupByCategoryResponse> videos = this.getVideosGroupByCategory.execute(videoGroupByCategoryRequest);
        if (CollectionUtils.isEmpty(videos)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.videoGroupByCategoryListResponseToVideoGroupByCategoryListResponseDtoConverter.convert(videos));
    }

}
