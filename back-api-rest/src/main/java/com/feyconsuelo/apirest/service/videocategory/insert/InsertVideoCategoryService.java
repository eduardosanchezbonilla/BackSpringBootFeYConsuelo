package com.feyconsuelo.apirest.service.videocategory.insert;

import com.feyconsuelo.apirest.converter.videocategory.VideoCategoryRequestDtoToVideoCategoryRequestConverter;
import com.feyconsuelo.domain.usecase.videocategory.InsertVideoCategory;
import com.feyconsuelo.openapi.model.VideoCategoryRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertVideoCategoryService {

    private final InsertVideoCategory insertVideoCategory;

    private final VideoCategoryRequestDtoToVideoCategoryRequestConverter videoCategoryRequestDtoToVideoCategoryRequestConverter;

    public ResponseEntity<Void> insertVideoCategory(final VideoCategoryRequestDto videoCategoryRequestDto) {
        this.insertVideoCategory.execute(
                this.videoCategoryRequestDtoToVideoCategoryRequestConverter.convert(videoCategoryRequestDto)
        );
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }
}
