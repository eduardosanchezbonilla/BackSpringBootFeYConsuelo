package com.feyconsuelo.infrastructure.converter.partituregroup;

import com.feyconsuelo.domain.model.partituregroup.PartitureGroupRequest;
import com.feyconsuelo.infrastructure.entities.partituregroup.PartitureGroupEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class PartitureGroupRequestToPartitureGroupEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public PartitureGroupEntity convert(final PartitureGroupRequest partitureGroupRequest) {
        return PartitureGroupEntity.builder()
                .name(partitureGroupRequest.getName())
                .googleId(partitureGroupRequest.getGoogleId())
                .modifiedUser(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public PartitureGroupEntity updateEntity(final PartitureGroupEntity partitureGroupEntity,
                                             final PartitureGroupRequest partitureGroupRequest) {
        partitureGroupEntity.setName(partitureGroupRequest.getName());
        partitureGroupEntity.setGoogleId(partitureGroupRequest.getGoogleId());
        partitureGroupEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return partitureGroupEntity;
    }

    public PartitureGroupEntity deleteEntity(final PartitureGroupEntity partitureGroupEntity) {
        partitureGroupEntity.setDeleteDate(LocalDateTime.now());
        partitureGroupEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return partitureGroupEntity;
    }
}
