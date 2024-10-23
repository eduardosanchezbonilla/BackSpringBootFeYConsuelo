package com.feyconsuelo.apirest.converter.partiture;

import com.feyconsuelo.domain.model.partiture.PartitureResponse;
import com.feyconsuelo.openapi.model.PartitureResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class PartitureResponseListToPartitureResponseDtoListConverter {

    private final PartitureResponseToPartitureResponseDtoConverter partitureResponseToPartitureResponseDtoConverter;

    public List<PartitureResponseDto> convert(final List<PartitureResponse> partitureResponseList) {
        if (CollectionUtils.isEmpty(partitureResponseList)) {
            return List.of();
        }
        return partitureResponseList.stream()
                .map(this.partitureResponseToPartitureResponseDtoConverter::convert)
                .sorted(Comparator.comparing(PartitureResponseDto::getName))
                .toList();
    }

}
