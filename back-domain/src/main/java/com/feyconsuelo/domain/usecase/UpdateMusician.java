package com.feyconsuelo.domain.usecase;

import com.feyconsuelo.domain.entity.musician.Musician;

public interface UpdateMusician {

    Musician execute(String musicianId, Musician musician);

}
