package com.feyconsuelo.infrastructure.converter.partituregroup;

import com.feyconsuelo.domain.model.partituregroup.PartitureGroupResponse;
import com.feyconsuelo.infrastructure.entities.partituregroup.PartitureGroupEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class PartitureGroupEntityToPartitureGroupResponseConverter {

    public PartitureGroupResponse convert(final PartitureGroupEntity partitureGroupEntity) {
        return PartitureGroupResponse.builder()
                .id(partitureGroupEntity.getId())
                .name(partitureGroupEntity.getName())
                .googleId(partitureGroupEntity.getGoogleId())
                .deleteDate(partitureGroupEntity.getDeleteDate())
                .build();
    }
}
