package com.feyconsuelo.application.repository;

import com.feyconsuelo.domain.entity.musician.FindMusiciansRequest;
import com.feyconsuelo.domain.entity.musician.Musician;

import java.util.List;
import java.util.Optional;

public interface MusicianRepository {

    void delete(String musicianId);

    List<Musician> find(FindMusiciansRequest request);

    List<Musician> getAll();

    Optional<Musician> get(String musicianId);

    Musician save(Musician musician);

}
