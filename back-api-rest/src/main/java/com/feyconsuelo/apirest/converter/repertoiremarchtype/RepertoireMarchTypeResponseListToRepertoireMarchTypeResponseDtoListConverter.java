package com.feyconsuelo.apirest.converter.repertoiremarchtype;

import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;
import com.feyconsuelo.openapi.model.RepertoireMarchTypeResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchTypeResponseListToRepertoireMarchTypeResponseDtoListConverter {

    private final RepertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter repertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter;

    public List<RepertoireMarchTypeResponseDto> convert(final List<RepertoireMarchTypeResponse> repertoireMarchTypeResponseList) {
        if (CollectionUtils.isEmpty(repertoireMarchTypeResponseList)) {
            return List.of();
        }
        return repertoireMarchTypeResponseList.stream()
                .map(this.repertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter::convert)
                .sorted(Comparator.comparing(RepertoireMarchTypeResponseDto::getOrder))
                .toList();
    }

}
