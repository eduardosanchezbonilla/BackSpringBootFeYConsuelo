package com.feyconsuelo.domain.usecase.contractgroup;

import com.feyconsuelo.domain.model.contractgroup.ContractGroupRequest;

public interface UpdateContractGroup {

    void execute(Long contractGroupId, ContractGroupRequest contractGroupRequest);

}
