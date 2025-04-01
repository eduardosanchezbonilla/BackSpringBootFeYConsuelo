package com.feyconsuelo.application.usecase.contractgroup;

import com.feyconsuelo.application.service.contractgroup.ContractGroupService;
import com.feyconsuelo.domain.model.contractgroup.ContractGroupRequest;
import com.feyconsuelo.domain.usecase.contractgroup.InsertContractGroup;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertContractGroupImpl implements InsertContractGroup {

    private final ContractGroupService contractGroupService;

    @Override
    public void execute(final ContractGroupRequest contractGroupRequest) {
        this.contractGroupService.insert(contractGroupRequest);
    }

}
