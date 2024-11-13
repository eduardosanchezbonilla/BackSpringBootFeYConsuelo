package com.feyconsuelo.apirest.service.videocategory.delete;

import com.feyconsuelo.domain.usecase.videocategory.DeleteVideoCategory;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteVideoCategoryService {

    private final DeleteVideoCategory deleteVideoCategory;

    public ResponseEntity<Void> deleteVideoCategory(final Long videoCategoryId) {
        this.deleteVideoCategory.execute(videoCategoryId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
