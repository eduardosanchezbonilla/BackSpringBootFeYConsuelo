package com.feyconsuelo.apirest.converter.contract;

import com.feyconsuelo.domain.model.contract.ContractResponse;
import com.feyconsuelo.openapi.model.ContractResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class ContractResponseToContractResponseDtoConverter {

    public ContractResponseDto convert(final ContractResponse contractResponse) {
        return ContractResponseDto.builder()
                .name(contractResponse.getName())
                .googleId(contractResponse.getGoogleId())
                .content(contractResponse.getContent())
                .mimeType(contractResponse.getMimeType())
                .build();
    }

}
