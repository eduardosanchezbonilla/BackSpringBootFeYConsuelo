package com.feyconsuelo.apirest.service.videocategory;

import com.feyconsuelo.apirest.service.videocategory.delete.DeleteVideoCategoryService;
import com.feyconsuelo.apirest.service.videocategory.insert.InsertVideoCategoryService;
import com.feyconsuelo.apirest.service.videocategory.query.GetVideoCategoryService;
import com.feyconsuelo.apirest.service.videocategory.update.UpdateVideoCategoryService;
import com.feyconsuelo.openapi.api.VideoCategoryControllerApiDelegate;
import com.feyconsuelo.openapi.model.VideoCategoryGroupByYearResponseDto;
import com.feyconsuelo.openapi.model.VideoCategoryRequestDto;
import com.feyconsuelo.openapi.model.VideoCategoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class VideoCategoryApiService implements VideoCategoryControllerApiDelegate {

    private final DeleteVideoCategoryService deleteVideoCategoryService;
    private final InsertVideoCategoryService insertVideoCategoryService;
    private final UpdateVideoCategoryService updateVideoCategoryService;
    private final GetVideoCategoryService getVideoCategoryService;

    @Override
    public ResponseEntity<Void> deleteVideoCategory(final Long videoCategoryId) {
        return this.deleteVideoCategoryService.deleteVideoCategory(videoCategoryId);
    }

    @Override
    public ResponseEntity<Void> insertVideoCategory(final VideoCategoryRequestDto videoCategoryRequestDto) {
        return this.insertVideoCategoryService.insertVideoCategory(videoCategoryRequestDto);
    }

    @Override
    public ResponseEntity<Void> updateVideoCategory(final Long videoCategoryId,
                                                    final VideoCategoryRequestDto videoCategoryRequestDto) {
        return this.updateVideoCategoryService.updateVideoCategory(videoCategoryId, videoCategoryRequestDto);
    }


    @Override
    public ResponseEntity<List<VideoCategoryResponseDto>> getAllVideoCategories() {
        return this.getVideoCategoryService.getAllVideoCategories();
    }

    @Override
    public ResponseEntity<VideoCategoryResponseDto> getVideoCategory(final Long videoCategoryId) {
        return this.getVideoCategoryService.getVideoCategory(videoCategoryId);
    }

    @Override
    public ResponseEntity<VideoCategoryResponseDto> getVideoCategoryImage(final Long videoCategoryId) {
        return this.getVideoCategoryService.getVideoCategoryImage(videoCategoryId);
    }

    @Override
    public ResponseEntity<List<VideoCategoryGroupByYearResponseDto>> getAllVideoCategoriesGroupByYear(final Boolean onlyPublic) {
        return this.getVideoCategoryService.getAllVideoCategoriesGroupByYear(onlyPublic == null ? Boolean.FALSE : onlyPublic);
    }

}
