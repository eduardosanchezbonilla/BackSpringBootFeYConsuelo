package com.feyconsuelo.domain.usecase.partiture;

import com.feyconsuelo.domain.model.partiture.PartitureRequest;
import com.feyconsuelo.domain.model.partiture.PartitureResponse;

import java.util.List;

public interface GetAllPartitures {

    List<PartitureResponse> execute(final PartitureRequest partitureRequest);

}
