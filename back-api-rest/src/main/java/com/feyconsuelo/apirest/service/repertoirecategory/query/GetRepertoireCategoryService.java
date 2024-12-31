package com.feyconsuelo.apirest.service.repertoirecategory.query;

import com.feyconsuelo.apirest.converter.repertoirecategory.RepertoireCategoryResponseListToRepertoireCategoryResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.repertoirecategory.RepertoireCategoryResponseToRepertoireCategoryResponseDtoConverter;
import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryResponse;
import com.feyconsuelo.domain.usecase.repertoirecategory.GetAllRepertoireCategories;
import com.feyconsuelo.domain.usecase.repertoirecategory.GetRepertoireCategory;
import com.feyconsuelo.openapi.model.RepertoireCategoryResponseDto;
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
public class GetRepertoireCategoryService {

    private final GetAllRepertoireCategories getAllRepertoireCategories;

    private final GetRepertoireCategory getRepertoireCategory;

    private final RepertoireCategoryResponseToRepertoireCategoryResponseDtoConverter repertoireCategoryResponseToRepertoireCategoryResponseDtoConverter;

    private final RepertoireCategoryResponseListToRepertoireCategoryResponseDtoListConverter repertoireCategoryResponseListToRepertoireCategoryResponseDtoListConverter;

    public ResponseEntity<List<RepertoireCategoryResponseDto>> getAllRepertoireCategories() {
        final List<RepertoireCategoryResponse> repertoireCategoryResponseList = this.getAllRepertoireCategories.execute();
        if (CollectionUtils.isEmpty(repertoireCategoryResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.repertoireCategoryResponseListToRepertoireCategoryResponseDtoListConverter.convert(repertoireCategoryResponseList));
    }

    public ResponseEntity<RepertoireCategoryResponseDto> getRepertoireCategory(final Long repertoireCategoryId) {
        final Optional<RepertoireCategoryResponseDto> repertoireCategoryResponse = this.getRepertoireCategory.execute(repertoireCategoryId).map(this.repertoireCategoryResponseToRepertoireCategoryResponseDtoConverter::convert);
        return repertoireCategoryResponse.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
