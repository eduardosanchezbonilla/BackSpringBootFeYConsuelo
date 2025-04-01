package com.feyconsuelo.domain.usecase.contract;

import com.feyconsuelo.domain.model.contract.ContractResponse;

import java.util.Optional;

public interface DownloadContract {

    Optional<ContractResponse> execute(final String contractGoogleId);

}
