package com.feyconsuelo.domain.usecase.contract;

import com.feyconsuelo.domain.model.contract.ContractRequest;
import com.feyconsuelo.domain.model.contract.ContractResponse;

import java.util.List;

public interface GetAllContracts {

    List<ContractResponse> execute(final ContractRequest contractRequest);

}
