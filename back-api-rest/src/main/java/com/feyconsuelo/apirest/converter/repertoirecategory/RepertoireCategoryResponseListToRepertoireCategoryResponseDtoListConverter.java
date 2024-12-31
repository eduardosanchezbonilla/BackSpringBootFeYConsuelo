package com.feyconsuelo.apirest.converter.repertoirecategory;

import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryResponse;
import com.feyconsuelo.openapi.model.RepertoireCategoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireCategoryResponseListToRepertoireCategoryResponseDtoListConverter {

    private final RepertoireCategoryResponseToRepertoireCategoryResponseDtoConverter repertoireCategoryResponseToRepertoireCategoryResponseDtoConverter;

    public List<RepertoireCategoryResponseDto> convert(final List<RepertoireCategoryResponse> repertoireCategoryResponseList) {
        if (CollectionUtils.isEmpty(repertoireCategoryResponseList)) {
            return List.of();
        }
        return repertoireCategoryResponseList.stream()
                .map(this.repertoireCategoryResponseToRepertoireCategoryResponseDtoConverter::convert)
                .sorted(Comparator.comparing(RepertoireCategoryResponseDto::getOrder))
                .toList();
    }

}
