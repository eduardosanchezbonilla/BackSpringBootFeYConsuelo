package com.feyconsuelo.infrastructure.converter.repertoirecategory;

import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryResponse;
import com.feyconsuelo.infrastructure.entities.repertoirecategory.RepertoireCategoryEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireCategoryEntityToRepertoireCategoryResponseConverter {

    public RepertoireCategoryResponse convert(final RepertoireCategoryEntity repertoireCategoryEntity) {
        return RepertoireCategoryResponse.builder()
                .id(repertoireCategoryEntity.getId())
                .name(repertoireCategoryEntity.getName())
                .order(repertoireCategoryEntity.getOrder())
                .image(repertoireCategoryEntity.getImage())
                .deleteDate(repertoireCategoryEntity.getRepertoireDeleteDate())
                .current(repertoireCategoryEntity.getCurrent())
                .build();
    }

}
