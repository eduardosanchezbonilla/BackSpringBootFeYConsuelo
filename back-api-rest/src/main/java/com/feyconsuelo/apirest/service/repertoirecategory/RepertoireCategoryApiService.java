package com.feyconsuelo.apirest.service.repertoirecategory;

import com.feyconsuelo.apirest.service.repertoirecategory.delete.DeleteRepertoireCategoryService;
import com.feyconsuelo.apirest.service.repertoirecategory.insert.InsertRepertoireCategoryService;
import com.feyconsuelo.apirest.service.repertoirecategory.query.GetRepertoireCategoryService;
import com.feyconsuelo.apirest.service.repertoirecategory.update.UpdateRepertoireCategoryService;
import com.feyconsuelo.openapi.api.RepertoireCategoryControllerApiDelegate;
import com.feyconsuelo.openapi.model.RepertoireCategoryRequestDto;
import com.feyconsuelo.openapi.model.RepertoireCategoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class RepertoireCategoryApiService implements RepertoireCategoryControllerApiDelegate {

    private final DeleteRepertoireCategoryService deleteRepertoireCategoryService;
    private final InsertRepertoireCategoryService insertRepertoireCategoryService;
    private final UpdateRepertoireCategoryService updateRepertoireCategoryService;
    private final GetRepertoireCategoryService getRepertoireCategoryService;

    @Override
    public ResponseEntity<Void> deleteRepertoireCategory(final Long repertoireCategoryId) {
        return this.deleteRepertoireCategoryService.deleteRepertoireCategory(repertoireCategoryId);
    }

    @Override
    public ResponseEntity<Void> insertRepertoireCategory(final RepertoireCategoryRequestDto repertoireCategoryRequestDto) {
        return this.insertRepertoireCategoryService.insertRepertoireCategory(repertoireCategoryRequestDto);
    }

    @Override
    public ResponseEntity<Void> updateRepertoireCategory(final Long repertoireCategoryId,
                                                         final RepertoireCategoryRequestDto repertoireCategoryRequestDto) {
        return this.updateRepertoireCategoryService.updateRepertoireCategory(repertoireCategoryId, repertoireCategoryRequestDto);
    }


    @Override
    public ResponseEntity<List<RepertoireCategoryResponseDto>> getAllRepertoireCategories() {
        return this.getRepertoireCategoryService.getAllRepertoireCategories();
    }

    @Override
    public ResponseEntity<RepertoireCategoryResponseDto> getRepertoireCategory(final Long repertoireCategoryId) {
        return this.getRepertoireCategoryService.getRepertoireCategory(repertoireCategoryId);
    }

}
