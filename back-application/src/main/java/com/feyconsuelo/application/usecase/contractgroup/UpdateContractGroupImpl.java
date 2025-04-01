package com.feyconsuelo.application.usecase.contractgroup;

import com.feyconsuelo.application.service.contractgroup.ContractGroupService;
import com.feyconsuelo.domain.model.contractgroup.ContractGroupRequest;
import com.feyconsuelo.domain.usecase.contractgroup.UpdateContractGroup;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdateContractGroupImpl implements UpdateContractGroup {

    private final ContractGroupService contractGroupService;

    @Override
    public void execute(final Long ContractGroupId, final ContractGroupRequest contractGroupRequest) {
        this.contractGroupService.update(ContractGroupId, contractGroupRequest);
    }

}
