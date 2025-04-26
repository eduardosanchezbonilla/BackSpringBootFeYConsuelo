package com.feyconsuelo.infrastructure.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchProjectionToRepertoireMarchResponseConverter {

    private final RepertoireCategoryStringToRepertoireCategoryResponseConverter repertoireCategoryStringToRepertoireCategoryResponseConverter;
    private final RepertoireTypeStringToRepertoireMarchTypeResponseConverter repertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter;
    private final RepertoireMarchSoloStringToRepertoireMarchSoloListResponseConverter repertoireMarchSoloStringToRepertoireMarchSoloListResponseConverter;

    public RepertoireMarchResponse convert(final RepertoireMarchProjection repertoireMarchEntity, final Integer order, final Integer numbers, final Boolean returnSolos) {
        return RepertoireMarchResponse.builder()
                .id(repertoireMarchEntity.getId())
                .categoryId(repertoireMarchEntity.getCategoryId())
                .category(this.repertoireCategoryStringToRepertoireCategoryResponseConverter.convert(repertoireMarchEntity.getCategory()))
                .typeId(repertoireMarchEntity.getTypeId())
                .type(this.repertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter.convert(repertoireMarchEntity.getType()))
                .name(repertoireMarchEntity.getName())
                .author(repertoireMarchEntity.getAuthor())
                .description(repertoireMarchEntity.getDescription())
                .youtubeId(repertoireMarchEntity.getYoutubeId())
                .deleteDate(repertoireMarchEntity.getDeleteDate())
                .order(order)
                .numbers(numbers)
                .repertoireMarchSolos(
                        Boolean.FALSE.equals(returnSolos) ? null :
                                this.repertoireMarchSoloStringToRepertoireMarchSoloListResponseConverter.convert(repertoireMarchEntity.getSolos())
                )
                .build();
    }

}
