package com.feyconsuelo.domain.usecase;

import com.feyconsuelo.domain.entity.musician.FindMusiciansRequest;
import com.feyconsuelo.domain.entity.musician.Musician;

import java.util.List;

public interface FindMusicians {

    List<Musician> execute(FindMusiciansRequest request);

}
