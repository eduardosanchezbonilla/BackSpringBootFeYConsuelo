package com.feyconsuelo.domain.usecase.contract;

import com.feyconsuelo.domain.model.contract.ContractRequest;
import com.feyconsuelo.domain.model.contract.ContractResponse;

import java.util.Optional;

public interface UploadContract {

    Optional<ContractResponse> execute(final ContractRequest contractRequest);

}
