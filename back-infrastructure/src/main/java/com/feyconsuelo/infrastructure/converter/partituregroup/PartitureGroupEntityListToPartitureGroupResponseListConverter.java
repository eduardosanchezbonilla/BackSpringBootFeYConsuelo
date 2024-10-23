package com.feyconsuelo.infrastructure.converter.partituregroup;

import com.feyconsuelo.domain.model.partituregroup.PartitureGroupResponse;
import com.feyconsuelo.infrastructure.entities.partituregroup.PartitureGroupEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class PartitureGroupEntityListToPartitureGroupResponseListConverter {

    private final PartitureGroupEntityToPartitureGroupResponseConverter partitureGroupEntityToPartitureGroupResponseConverter;

    public List<PartitureGroupResponse> convert(final List<PartitureGroupEntity> partitureGroupEntityList) {
        if (CollectionUtils.isEmpty(partitureGroupEntityList)) {
            return List.of();
        }
        return partitureGroupEntityList.stream()
                .map(this.partitureGroupEntityToPartitureGroupResponseConverter::convert)
                .toList();
    }
}
