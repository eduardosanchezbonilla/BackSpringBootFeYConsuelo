package com.feyconsuelo.application.usecase.contractgroup;

import com.feyconsuelo.application.service.contractgroup.ContractGroupService;
import com.feyconsuelo.domain.usecase.contractgroup.DeleteContractGroup;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteContractGroupImpl implements DeleteContractGroup {

    private final ContractGroupService contractGroupService;

    @Override
    public void execute(final Long contractGroupId) {
        this.contractGroupService.logicalDelete(contractGroupId);
    }

}
