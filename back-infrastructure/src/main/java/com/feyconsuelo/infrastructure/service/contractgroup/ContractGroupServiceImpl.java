package com.feyconsuelo.infrastructure.service.contractgroup;

import com.feyconsuelo.application.service.contractgroup.ContractGroupService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.contractgroup.ContractGroupRequest;
import com.feyconsuelo.domain.model.contractgroup.ContractGroupResponse;
import com.feyconsuelo.infrastructure.converter.contractgroup.ContractGroupEntityListToContractGroupResponseListConverter;
import com.feyconsuelo.infrastructure.converter.contractgroup.ContractGroupEntityToContractGroupResponseConverter;
import com.feyconsuelo.infrastructure.converter.contractgroup.ContractGroupRequestToContractGroupEntityConverter;
import com.feyconsuelo.infrastructure.entities.contractgroup.ContractGroupEntity;
import com.feyconsuelo.infrastructure.repository.ContractGroupRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class ContractGroupServiceImpl implements ContractGroupService {

    private final ContractGroupRepository contractGroupRepository;
    private final ContractGroupRequestToContractGroupEntityConverter contractGroupRequestToContractGroupEntityConverter;
    private final ContractGroupEntityListToContractGroupResponseListConverter contractGroupEntityListToContractGroupResponseListConverter;
    private final ContractGroupEntityToContractGroupResponseConverter contractGroupEntityToContractGroupResponseConverter;

    @Override
    public void delete(final Long contractGroupId) {
        this.contractGroupRepository.deleteById(contractGroupId);
    }

    @Override
    public void logicalDelete(final Long contractGroupId) {

        final var contractGroup = this.contractGroupRepository.findContractGroupActiveById(contractGroupId);

        if (contractGroup.isEmpty()) {
            throw new NotFoundException("No existe el grupo de contratos desea eliminar");
        }

        this.contractGroupRepository.save(this.contractGroupRequestToContractGroupEntityConverter.deleteEntity(contractGroup.get()));
    }

    @Override
    public List<ContractGroupResponse> getAll(final List<Long> contractGroupIdList,
                                              final Boolean allContractGroups
    ) {
        final List<ContractGroupEntity> contractGroups = this.contractGroupRepository.findAllActives(contractGroupIdList, allContractGroups);
        return this.contractGroupEntityListToContractGroupResponseListConverter.convert(contractGroups);
    }

    @Override
    public Optional<ContractGroupResponse> get(final Long contractGroupId) {
        final var contractGroup = this.contractGroupRepository.findContractGroupActiveById(contractGroupId);
        return contractGroup.map(this.contractGroupEntityToContractGroupResponseConverter::convert);
    }

    @Override
    public void insert(final ContractGroupRequest contractGroupRequest) {
        this.contractGroupRepository.save(
                this.contractGroupRequestToContractGroupEntityConverter.convert(contractGroupRequest)
        );
    }

    @Override
    public void update(final Long contractGroupId,
                       final ContractGroupRequest contractGroupRequest) {

        final var contractGroup = this.contractGroupRepository.findContractGroupActiveById(contractGroupId);

        if (contractGroup.isEmpty()) {
            throw new NotFoundException("No existe el grupo de contratos desea modificar");
        }

        contractGroup.get().setName(contractGroupRequest.getName());
        contractGroup.get().setGoogleId(contractGroupRequest.getGoogleId());
        this.contractGroupRepository.save(
                this.contractGroupRequestToContractGroupEntityConverter.updateEntity(contractGroup.get(), contractGroupRequest)
        );
    }

}
