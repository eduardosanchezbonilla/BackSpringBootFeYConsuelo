package com.feyconsuelo.infrastructure.converter.contractgroup;

import com.feyconsuelo.domain.model.contractgroup.ContractGroupResponse;
import com.feyconsuelo.infrastructure.entities.contractgroup.ContractGroupEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class ContractGroupEntityToContractGroupResponseConverter {

    public ContractGroupResponse convert(final ContractGroupEntity contractGroupEntity) {
        return ContractGroupResponse.builder()
                .id(contractGroupEntity.getId())
                .name(contractGroupEntity.getName())
                .googleId(contractGroupEntity.getGoogleId())
                .deleteDate(contractGroupEntity.getDeleteDate())
                .build();
    }
}
