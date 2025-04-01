package com.feyconsuelo.apirest.converter.contractgroup;

import com.feyconsuelo.domain.model.contractgroup.ContractGroupRequest;
import com.feyconsuelo.openapi.model.ContractGroupRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class ContractGroupRequestDtoToContractGroupRequestConverter {

    public ContractGroupRequest convert(final ContractGroupRequestDto contractGroupRequestDto) {
        return ContractGroupRequest.builder()
                .name(contractGroupRequestDto.getName().toUpperCase())
                .googleId(contractGroupRequestDto.getGoogleId())
                .build();
    }

}
