package com.feyconsuelo.infrastructure.converter.repertoirecategory;

import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryResponse;
import com.feyconsuelo.infrastructure.entities.repertoirecategory.RepertoireCategoryEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireCategoryEntityListToRepertoireCategoryResponseListConverter {

    private final RepertoireCategoryEntityToRepertoireCategoryResponseConverter repertoireCategoryEntityToRepertoireCategoryResponseConverter;

    public List<RepertoireCategoryResponse> convert(final List<RepertoireCategoryEntity> repertoireCategoryEntityList) {
        if (CollectionUtils.isEmpty(repertoireCategoryEntityList)) {
            return List.of();
        }
        return repertoireCategoryEntityList.stream()
                .map(this.repertoireCategoryEntityToRepertoireCategoryResponseConverter::convert)
                .toList();
    }
}
