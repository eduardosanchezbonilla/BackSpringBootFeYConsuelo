package com.feyconsuelo.infrastructure.converter.contractgroup;

import com.feyconsuelo.domain.model.contractgroup.ContractGroupResponse;
import com.feyconsuelo.infrastructure.entities.contractgroup.ContractGroupEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class ContractGroupEntityListToContractGroupResponseListConverter {

    private final ContractGroupEntityToContractGroupResponseConverter contractGroupEntityToContractGroupResponseConverter;

    public List<ContractGroupResponse> convert(final List<ContractGroupEntity> contractGroupEntityList) {
        if (CollectionUtils.isEmpty(contractGroupEntityList)) {
            return List.of();
        }
        return contractGroupEntityList.stream()
                .map(this.contractGroupEntityToContractGroupResponseConverter::convert)
                .toList();
    }
}
