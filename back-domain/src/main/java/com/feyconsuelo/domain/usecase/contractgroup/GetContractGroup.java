package com.feyconsuelo.domain.usecase.contractgroup;

import com.feyconsuelo.domain.model.contractgroup.ContractGroupResponse;

import java.util.Optional;

public interface GetContractGroup {

    Optional<ContractGroupResponse> execute(Long contractGroupId);

}
