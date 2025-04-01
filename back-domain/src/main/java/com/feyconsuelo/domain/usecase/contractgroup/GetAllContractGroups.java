package com.feyconsuelo.domain.usecase.contractgroup;

import com.feyconsuelo.domain.model.contractgroup.ContractGroupResponse;

import java.util.List;

public interface GetAllContractGroups {

    List<ContractGroupResponse> execute();

}
