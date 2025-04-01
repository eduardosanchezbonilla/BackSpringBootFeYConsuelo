package com.feyconsuelo.apirest.converter.contractgroup;

import com.feyconsuelo.domain.model.contractgroup.ContractGroupResponse;
import com.feyconsuelo.openapi.model.ContractGroupResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class ContractGroupResponseListToContractGroupResponseDtoListConverter {

    private final ContractGroupResponseToContractGroupResponseDtoConverter contractGroupResponseToContractGroupResponseDtoConverter;

    public List<ContractGroupResponseDto> convert(final List<ContractGroupResponse> contractGroupResponseList) {
        if (CollectionUtils.isEmpty(contractGroupResponseList)) {
            return List.of();
        }
        return contractGroupResponseList.stream()
                .map(this.contractGroupResponseToContractGroupResponseDtoConverter::convert)
                .sorted(Comparator.comparing(ContractGroupResponseDto::getName))
                .toList();
    }

}
