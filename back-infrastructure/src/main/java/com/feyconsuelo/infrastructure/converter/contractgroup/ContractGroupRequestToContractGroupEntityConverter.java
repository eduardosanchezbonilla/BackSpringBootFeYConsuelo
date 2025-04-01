package com.feyconsuelo.infrastructure.converter.contractgroup;

import com.feyconsuelo.domain.model.contractgroup.ContractGroupRequest;
import com.feyconsuelo.infrastructure.entities.contractgroup.ContractGroupEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class ContractGroupRequestToContractGroupEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public ContractGroupEntity convert(final ContractGroupRequest contractGroupRequest) {
        return ContractGroupEntity.builder()
                .name(contractGroupRequest.getName())
                .googleId(contractGroupRequest.getGoogleId())
                .modifiedUser(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public ContractGroupEntity updateEntity(final ContractGroupEntity contractGroupEntity,
                                            final ContractGroupRequest contractGroupRequest) {
        contractGroupEntity.setName(contractGroupRequest.getName());
        contractGroupEntity.setGoogleId(contractGroupRequest.getGoogleId());
        contractGroupEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return contractGroupEntity;
    }

    public ContractGroupEntity deleteEntity(final ContractGroupEntity contractGroupEntity) {
        contractGroupEntity.setDeleteDate(LocalDateTime.now());
        contractGroupEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return contractGroupEntity;
    }
}
