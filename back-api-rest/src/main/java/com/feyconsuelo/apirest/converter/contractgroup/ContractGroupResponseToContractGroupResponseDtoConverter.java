package com.feyconsuelo.apirest.converter.contractgroup;

import com.feyconsuelo.domain.model.contractgroup.ContractGroupResponse;
import com.feyconsuelo.openapi.model.ContractGroupResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class ContractGroupResponseToContractGroupResponseDtoConverter {

    public ContractGroupResponseDto convert(final ContractGroupResponse contractGroupResponse) {
        return ContractGroupResponseDto.builder()
                .id(contractGroupResponse.getId())
                .name(contractGroupResponse.getName())
                .googleId(contractGroupResponse.getGoogleId())
                .build();
    }

}
