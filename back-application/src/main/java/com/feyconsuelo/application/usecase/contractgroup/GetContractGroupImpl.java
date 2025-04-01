package com.feyconsuelo.application.usecase.contractgroup;

import com.feyconsuelo.application.service.contractgroup.ContractGroupService;
import com.feyconsuelo.domain.model.contractgroup.ContractGroupResponse;
import com.feyconsuelo.domain.usecase.contractgroup.GetContractGroup;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetContractGroupImpl implements GetContractGroup {

    private final ContractGroupService contractGroupService;

    @Override
    public Optional<ContractGroupResponse> execute(final Long contractGroupId) {
        return this.contractGroupService.get(contractGroupId);
    }
}
