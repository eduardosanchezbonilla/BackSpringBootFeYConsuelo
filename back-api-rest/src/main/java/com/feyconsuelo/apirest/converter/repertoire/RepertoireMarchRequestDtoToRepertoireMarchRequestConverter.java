package com.feyconsuelo.apirest.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchRequest;
import com.feyconsuelo.openapi.model.RepertoireMarchRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchRequestDtoToRepertoireMarchRequestConverter {

    private final RepertoireMarchSoloDtoToRepertoireMarchSoloConverter repertoireMarchSoloDtoToRepertoireMarchSoloConverter;

    public RepertoireMarchRequest convert(final RepertoireMarchRequestDto repertoireMarchRequestDto) {
        return RepertoireMarchRequest.builder()
                .categoryId(repertoireMarchRequestDto.getCategoryId())
                .typeId(repertoireMarchRequestDto.getTypeId())
                .name(repertoireMarchRequestDto.getName())
                .author(repertoireMarchRequestDto.getAuthor())
                .description(repertoireMarchRequestDto.getDescription())
                .image(repertoireMarchRequestDto.getImage())
                .youtubeId(repertoireMarchRequestDto.getYoutubeId())
                .repertoireMarchSolos(
                        CollectionUtils.isEmpty(repertoireMarchRequestDto.getRepertoireMarchSolos()) ?
                                List.of()
                                :
                                repertoireMarchRequestDto.getRepertoireMarchSolos().stream()
                                        .map(repertoireMarchSoloDtoToRepertoireMarchSoloConverter::convert)
                                        .toList()
                )
                .build();
    }

}
