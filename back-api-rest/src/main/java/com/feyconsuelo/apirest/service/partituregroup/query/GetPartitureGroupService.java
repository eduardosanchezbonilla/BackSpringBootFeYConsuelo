package com.feyconsuelo.apirest.service.partituregroup.query;

import com.feyconsuelo.apirest.converter.partituregroup.PartitureGroupResponseListToPartitureGroupResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.partituregroup.PartitureGroupResponseToPartitureGroupResponseDtoConverter;
import com.feyconsuelo.domain.model.partituregroup.PartitureGroupResponse;
import com.feyconsuelo.domain.usecase.partituregroup.GetAllPartitureGroups;
import com.feyconsuelo.domain.usecase.partituregroup.GetPartitureGroup;
import com.feyconsuelo.openapi.model.PartitureGroupResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetPartitureGroupService {

    private final GetAllPartitureGroups getAllPartitureGroups;

    private final GetPartitureGroup getPartitureGroup;

    private final PartitureGroupResponseToPartitureGroupResponseDtoConverter partitureGroupResponseToPartitureGroupResponseDtoConverter;

    private final PartitureGroupResponseListToPartitureGroupResponseDtoListConverter partitureGroupResponseListToPartitureGroupResponseDtoListConverter;

    public ResponseEntity<List<PartitureGroupResponseDto>> getAllPartitureGroups() {
        final List<PartitureGroupResponse> partitureGroupResponseList = this.getAllPartitureGroups.execute();
        if (CollectionUtils.isEmpty(partitureGroupResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.partitureGroupResponseListToPartitureGroupResponseDtoListConverter.convert(partitureGroupResponseList));
    }

    public ResponseEntity<PartitureGroupResponseDto> getPartitureGroup(final Long partitureGroupId) {
        final Optional<PartitureGroupResponseDto> partitureGroupResponseDto = this.getPartitureGroup.execute(partitureGroupId).map(this.partitureGroupResponseToPartitureGroupResponseDtoConverter::convert);
        return partitureGroupResponseDto.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
