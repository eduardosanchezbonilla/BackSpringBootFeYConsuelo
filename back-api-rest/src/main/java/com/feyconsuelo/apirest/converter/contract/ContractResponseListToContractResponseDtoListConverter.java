package com.feyconsuelo.apirest.converter.contract;

import com.feyconsuelo.domain.model.contract.ContractResponse;
import com.feyconsuelo.openapi.model.ContractResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class ContractResponseListToContractResponseDtoListConverter {

    private final ContractResponseToContractResponseDtoConverter contractResponseToContractResponseDtoConverter;

    public List<ContractResponseDto> convert(final List<ContractResponse> contractResponseList) {
        if (CollectionUtils.isEmpty(contractResponseList)) {
            return List.of();
        }
        return contractResponseList.stream()
                .map(this.contractResponseToContractResponseDtoConverter::convert)
                .sorted(Comparator.comparing(ContractResponseDto::getName))
                .toList();
    }

}
