package com.feyconsuelo.apirest.service.videocategory.update;

import com.feyconsuelo.apirest.converter.videocategory.VideoCategoryRequestDtoToVideoCategoryRequestConverter;
import com.feyconsuelo.domain.usecase.videocategory.UpdateVideoCategory;
import com.feyconsuelo.openapi.model.VideoCategoryRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateVideoCategoryService {

    private final UpdateVideoCategory updateVideoCategory;

    private final VideoCategoryRequestDtoToVideoCategoryRequestConverter videoCategoryRequestDtoToVideoCategoryRequestConverter;

    public ResponseEntity<Void> updateVideoCategory(final Long videoCategoryId,
                                                    final VideoCategoryRequestDto videoCategoryRequestDto) {
        this.updateVideoCategory.execute(
                videoCategoryId,
                this.videoCategoryRequestDtoToVideoCategoryRequestConverter.convert(videoCategoryRequestDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();

    }
}
