package com.feyconsuelo.apirest.service.video;

import com.feyconsuelo.apirest.service.video.delete.DeleteVideoService;
import com.feyconsuelo.apirest.service.video.insert.InsertVideoService;
import com.feyconsuelo.apirest.service.video.query.GetVideoService;
import com.feyconsuelo.apirest.service.video.update.UpdateVideoService;
import com.feyconsuelo.domain.model.video.VideoGroupByCategoryRequest;
import com.feyconsuelo.openapi.api.VideoControllerApiDelegate;
import com.feyconsuelo.openapi.model.VideoGroupByCategoryResponseDto;
import com.feyconsuelo.openapi.model.VideoRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class VideoApiService implements VideoControllerApiDelegate {

    private final DeleteVideoService deleteVideoService;
    private final InsertVideoService insertVideoService;
    private final UpdateVideoService updateVideoService;
    private final GetVideoService getVideoService;

    @Override
    public ResponseEntity<Void> deleteVideo(final Long videoCategoryId) {
        return this.deleteVideoService.deleteVideo(videoCategoryId);
    }

    @Override
    public ResponseEntity<Void> insertVideo(final VideoRequestDto videoCategoryRequestDto) {
        return this.insertVideoService.insertVideo(videoCategoryRequestDto);
    }

    @Override
    public ResponseEntity<Void> updateVideo(final Long videoCategoryId,
                                            final VideoRequestDto videoCategoryRequestDto) {
        return this.updateVideoService.updateVideo(videoCategoryId, videoCategoryRequestDto);
    }

    @Override
    public ResponseEntity<List<VideoGroupByCategoryResponseDto>> getVideosGroupByCategory(final String name) {
        return this.getVideoService.getVideoGroupByCategory(
                VideoGroupByCategoryRequest.builder()
                        .name(name)
                        .build()
        );
    }


}
