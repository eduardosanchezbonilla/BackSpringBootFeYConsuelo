package com.feyconsuelo.domain.usecase;

import com.feyconsuelo.domain.entity.musician.Musician;

import java.util.List;

public interface GetAllMusicians {

    List<Musician> execute();

}
