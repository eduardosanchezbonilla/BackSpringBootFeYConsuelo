package com.feyconsuelo.apirest.converter.partituregroup;

import com.feyconsuelo.domain.model.partituregroup.PartitureGroupResponse;
import com.feyconsuelo.openapi.model.PartitureGroupResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class PartitureGroupResponseListToPartitureGroupResponseDtoListConverter {

    private final PartitureGroupResponseToPartitureGroupResponseDtoConverter partitureGroupResponseToPartitureGroupResponseDtoConverter;

    public List<PartitureGroupResponseDto> convert(final List<PartitureGroupResponse> partitureGroupResponseList) {
        if (CollectionUtils.isEmpty(partitureGroupResponseList)) {
            return List.of();
        }
        return partitureGroupResponseList.stream()
                .map(this.partitureGroupResponseToPartitureGroupResponseDtoConverter::convert)
                .sorted(Comparator.comparing(PartitureGroupResponseDto::getName))
                .toList();
    }

}
