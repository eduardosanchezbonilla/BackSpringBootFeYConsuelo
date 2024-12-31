package com.feyconsuelo.infrastructure.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.infrastructure.converter.repertoirecategory.RepertoireCategoryEntityToRepertoireCategoryResponseConverter;
import com.feyconsuelo.infrastructure.converter.repertorymarchtype.RepertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchEntityToRepertoireMarchResponseConverter {

    private final RepertoireCategoryEntityToRepertoireCategoryResponseConverter repertoireCategoryEntityToRepertoireCategoryResponseConverter;
    private final RepertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter repertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter;

    public RepertoireMarchResponse convert(final RepertoireMarchEntity repertoireMarchEntity) {
        return RepertoireMarchResponse.builder()
                .id(repertoireMarchEntity.getId())
                .categoryId(repertoireMarchEntity.getCategoryEntity().getId())
                .category(this.repertoireCategoryEntityToRepertoireCategoryResponseConverter.convert(repertoireMarchEntity.getCategoryEntity()))
                .typeId(repertoireMarchEntity.getTypeEntity().getId())
                .type(this.repertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter.convert(repertoireMarchEntity.getTypeEntity()))
                .name(repertoireMarchEntity.getName())
                .author(repertoireMarchEntity.getAuthor())
                .description(repertoireMarchEntity.getDescription())
                .image(repertoireMarchEntity.getImage())
                .youtubeId(repertoireMarchEntity.getYoutubeId())
                .deleteDate(repertoireMarchEntity.getRepertoireMarchDeleteDate())
                .build();
    }

}
