package com.feyconsuelo.infrastructure.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchSolo;
import com.feyconsuelo.infrastructure.converter.repertoirecategory.RepertoireCategoryEntityToRepertoireCategoryResponseConverter;
import com.feyconsuelo.infrastructure.converter.repertorymarchtype.RepertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchEntityToRepertoireMarchResponseConverter {

    private final RepertoireCategoryEntityToRepertoireCategoryResponseConverter repertoireCategoryEntityToRepertoireCategoryResponseConverter;
    private final RepertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter repertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter;
    private final RepertoireMarchSoloEntityToRepertoireMarchSoloConverter repertoireMarchSoloEntityToRepertoireMarchSoloResponseConverter;

    private List<RepertoireMarchSolo> getRepertoireMarchSolos(final RepertoireMarchEntity repertoireMarchEntity) {
        return CollectionUtils.isEmpty(repertoireMarchEntity.getSolos()) ?
                null :
                repertoireMarchEntity.getSolos().stream()
                        .map(this.repertoireMarchSoloEntityToRepertoireMarchSoloResponseConverter::convert)
                        .toList();
    }

    public RepertoireMarchResponse convert(final RepertoireMarchEntity repertoireMarchEntity, final Integer order, final Integer numbers, final Boolean returnSolos) {
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
                .order(order)
                .numbers(numbers)
                .repertoireMarchSolos(Boolean.FALSE.equals(returnSolos) ? null : this.getRepertoireMarchSolos(repertoireMarchEntity))
                .build();
    }

}
