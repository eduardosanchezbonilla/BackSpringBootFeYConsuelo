package com.feyconsuelo.application.service.musician;

import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.domain.model.musician.MusicianResponse;

import java.util.List;
import java.util.Optional;

public interface MusicianService {

    List<MusicianResponse> getAll();

    Optional<MusicianResponse> get(Long musicianId);

    Optional<MusicianResponse> getByDni(String dni);

    MusicianResponse insert(MusicianRequest musicianRequest);

    MusicianResponse update(Long musicianId, MusicianRequest musicianRequest);

    void delete(Long musicianId);

    void logicalDelete(Long musicianId);
}
