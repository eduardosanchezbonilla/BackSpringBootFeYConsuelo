package com.feyconsuelo.apirest.converter.partituregroup;

import com.feyconsuelo.domain.model.partituregroup.PartitureGroupResponse;
import com.feyconsuelo.openapi.model.PartitureGroupResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class PartitureGroupResponseToPartitureGroupResponseDtoConverter {

    public PartitureGroupResponseDto convert(final PartitureGroupResponse partitureGroupResponse) {
        return PartitureGroupResponseDto.builder()
                .id(partitureGroupResponse.getId())
                .name(partitureGroupResponse.getName())
                .googleId(partitureGroupResponse.getGoogleId())
                .build();
    }

}
