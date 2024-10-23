package com.feyconsuelo.domain.usecase.partiture;

import com.feyconsuelo.domain.model.partiture.PartitureResponse;

import java.util.Optional;

public interface DownloadPartiture {

    Optional<PartitureResponse> execute(final String partitureGoogleId);

}
