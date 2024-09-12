package com.feyconsuelo.domain.usecase.musician;

import com.feyconsuelo.domain.model.musician.MusicianResponse;

import java.util.Optional;

public interface GetMusician {

    Optional<MusicianResponse> execute(Long musicianId);

}
