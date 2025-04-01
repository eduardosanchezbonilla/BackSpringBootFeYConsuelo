package com.feyconsuelo.apirest.service.contractgroup.query;

import com.feyconsuelo.apirest.converter.contractgroup.ContractGroupResponseListToContractGroupResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.contractgroup.ContractGroupResponseToContractGroupResponseDtoConverter;
import com.feyconsuelo.domain.model.contractgroup.ContractGroupResponse;
import com.feyconsuelo.domain.usecase.contractgroup.GetAllContractGroups;
import com.feyconsuelo.domain.usecase.contractgroup.GetContractGroup;
import com.feyconsuelo.openapi.model.ContractGroupResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetContractGroupService {

    private final GetAllContractGroups getAllContractGroups;

    private final GetContractGroup getContractGroup;

    private final ContractGroupResponseToContractGroupResponseDtoConverter contractGroupResponseToContractGroupResponseDtoConverter;

    private final ContractGroupResponseListToContractGroupResponseDtoListConverter contractGroupResponseListToContractGroupResponseDtoListConverter;

    public ResponseEntity<List<ContractGroupResponseDto>> getAllContractGroups() {
        final List<ContractGroupResponse> contractGroupResponseList = this.getAllContractGroups.execute();
        if (CollectionUtils.isEmpty(contractGroupResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.contractGroupResponseListToContractGroupResponseDtoListConverter.convert(contractGroupResponseList));
    }

    public ResponseEntity<ContractGroupResponseDto> getContractGroup(final Long contractGroupId) {
        final Optional<ContractGroupResponseDto> contractGroupResponseDto = this.getContractGroup.execute(contractGroupId).map(this.contractGroupResponseToContractGroupResponseDtoConverter::convert);
        return contractGroupResponseDto.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
