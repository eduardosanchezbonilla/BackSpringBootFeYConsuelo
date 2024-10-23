package com.feyconsuelo.apirest.converter.partituregroup;

import com.feyconsuelo.domain.model.partituregroup.PartitureGroupRequest;
import com.feyconsuelo.openapi.model.PartitureGroupRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class PartitureGroupRequestDtoToPartitureGroupRequestConverter {

    public PartitureGroupRequest convert(final PartitureGroupRequestDto partitureGroupRequestDto) {
        return PartitureGroupRequest.builder()
                .name(partitureGroupRequestDto.getName().toUpperCase())
                .googleId(partitureGroupRequestDto.getGoogleId())
                .build();
    }

}
