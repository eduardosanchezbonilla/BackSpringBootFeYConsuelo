package com.feyconsuelo.infrastructure.service.partituregroup;

import com.feyconsuelo.application.service.partituregroup.PartitureGroupService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.partituregroup.PartitureGroupRequest;
import com.feyconsuelo.domain.model.partituregroup.PartitureGroupResponse;
import com.feyconsuelo.infrastructure.converter.partituregroup.PartitureGroupEntityListToPartitureGroupResponseListConverter;
import com.feyconsuelo.infrastructure.converter.partituregroup.PartitureGroupEntityToPartitureGroupResponseConverter;
import com.feyconsuelo.infrastructure.converter.partituregroup.PartitureGroupRequestToPartitureGroupEntityConverter;
import com.feyconsuelo.infrastructure.entities.partituregroup.PartitureGroupEntity;
import com.feyconsuelo.infrastructure.repository.PartitureGroupRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class PartitureGroupServiceImpl implements PartitureGroupService {

    private final PartitureGroupRepository partitureGroupRepository;
    private final PartitureGroupRequestToPartitureGroupEntityConverter partitureGroupRequestToPartitureGroupEntityConverter;
    private final PartitureGroupEntityListToPartitureGroupResponseListConverter partitureGroupEntityListToPartitureGroupResponseListConverter;
    private final PartitureGroupEntityToPartitureGroupResponseConverter partitureGroupEntityToPartitureGroupResponseConverter;

    @Override
    public void delete(final Long partitureGroupId) {
        this.partitureGroupRepository.deleteById(partitureGroupId);
    }

    @Override
    public void logicalDelete(final Long partitureGroupId) {

        final var partitureGroup = this.partitureGroupRepository.findPartitureGroupActiveById(partitureGroupId);

        if (partitureGroup.isEmpty()) {
            throw new NotFoundException("No existe el grupo de partituras desea eliminar");
        }

        this.partitureGroupRepository.save(this.partitureGroupRequestToPartitureGroupEntityConverter.deleteEntity(partitureGroup.get()));
    }

    @Override
    public List<PartitureGroupResponse> getAll(final List<Long> partitureGroupIdList,
                                               final Boolean allPartitureGroups
    ) {
        final List<PartitureGroupEntity> partitureGroups = this.partitureGroupRepository.findAllActives(partitureGroupIdList, allPartitureGroups);
        return this.partitureGroupEntityListToPartitureGroupResponseListConverter.convert(partitureGroups);
    }

    @Override
    public Optional<PartitureGroupResponse> get(final Long partitureGroupId) {
        final var partitureGroup = this.partitureGroupRepository.findPartitureGroupActiveById(partitureGroupId);
        return partitureGroup.map(this.partitureGroupEntityToPartitureGroupResponseConverter::convert);
    }

    @Override
    public void insert(final PartitureGroupRequest partitureGroupRequest) {
        this.partitureGroupRepository.save(
                this.partitureGroupRequestToPartitureGroupEntityConverter.convert(partitureGroupRequest)
        );
    }

    @Override
    public void update(final Long partitureGroupId,
                       final PartitureGroupRequest partitureGroupRequest) {

        final var partitureGroup = this.partitureGroupRepository.findPartitureGroupActiveById(partitureGroupId);

        if (partitureGroup.isEmpty()) {
            throw new NotFoundException("No existe el grupo de partituras desea modificar");
        }

        partitureGroup.get().setName(partitureGroupRequest.getName());
        partitureGroup.get().setGoogleId(partitureGroupRequest.getGoogleId());
        this.partitureGroupRepository.save(
                this.partitureGroupRequestToPartitureGroupEntityConverter.updateEntity(partitureGroup.get(), partitureGroupRequest)
        );
    }

}
