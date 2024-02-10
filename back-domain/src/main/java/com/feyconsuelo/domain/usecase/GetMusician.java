package com.feyconsuelo.domain.usecase;

import com.feyconsuelo.domain.entity.musician.Musician;

import java.util.Optional;

public interface GetMusician {

    Optional<Musician> execute(String musicianId);

}
