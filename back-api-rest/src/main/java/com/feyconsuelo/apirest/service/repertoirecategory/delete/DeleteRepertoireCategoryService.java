package com.feyconsuelo.apirest.service.repertoirecategory.delete;

import com.feyconsuelo.domain.usecase.repertoirecategory.DeleteRepertoireCategory;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteRepertoireCategoryService {

    private final DeleteRepertoireCategory deleteRepertoireCategory;

    public ResponseEntity<Void> deleteRepertoireCategory(final Long repertoireCategoryId) {
        this.deleteRepertoireCategory.execute(repertoireCategoryId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
