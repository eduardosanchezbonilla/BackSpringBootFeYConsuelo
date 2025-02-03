package com.feyconsuelo.apirest.converter.repertoire;

import com.feyconsuelo.apirest.converter.repertoirecategory.RepertoireCategoryResponseToRepertoireCategoryResponseDtoConverter;
import com.feyconsuelo.apirest.converter.repertoiremarchtype.RepertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.openapi.model.RepertoireMarchResponseDto;
import com.feyconsuelo.openapi.model.RepertoireMarchSoloDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchResponseToRepertoireMarchResponseDtoConverter {

    private final RepertoireCategoryResponseToRepertoireCategoryResponseDtoConverter repertoireCategoryResponseToRepertoireCategoryResponseDtoConverter;
    private final RepertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter repertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter;
    private final RepertoireMarchSoloToRepertoireMarchSoloDtoConverter repertoireMarchSoloToRepertoireMarchSoloDtoConverter;

    public RepertoireMarchResponseDto convert(final RepertoireMarchResponse repertoireMarchResponse) {
        return this.convert(repertoireMarchResponse, Boolean.TRUE);
    }

    public RepertoireMarchResponseDto convert(final RepertoireMarchResponse repertoireMarchResponse, final Boolean images) {
        return RepertoireMarchResponseDto.builder()
                .id(repertoireMarchResponse.getId())
                .category(this.repertoireCategoryResponseToRepertoireCategoryResponseDtoConverter.convert(repertoireMarchResponse.getCategory(), images))
                .type(this.repertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter.convert(repertoireMarchResponse.getType(), images))
                .name(repertoireMarchResponse.getName())
                .author(repertoireMarchResponse.getAuthor())
                .description(repertoireMarchResponse.getDescription())
                .image(repertoireMarchResponse.getImage())
                .youtubeId(repertoireMarchResponse.getYoutubeId())
                .checked(repertoireMarchResponse.getChecked() != null && repertoireMarchResponse.getChecked())
                .order(repertoireMarchResponse.getOrder())
                .numbers(repertoireMarchResponse.getNumbers())
                .repertoireMarchSolos(
                        CollectionUtils.isEmpty(repertoireMarchResponse.getRepertoireMarchSolos()) ?
                                List.of()
                                :
                                repertoireMarchResponse.getRepertoireMarchSolos().stream()
                                        .map(this.repertoireMarchSoloToRepertoireMarchSoloDtoConverter::convert)
                                        .sorted(Comparator.comparing(RepertoireMarchSoloDto::getOrder))
                                        .toList()
                )
                .build();
    }

}
