package com.feyconsuelo.domain.usecase.musician;

import com.feyconsuelo.domain.model.musician.MusicianChangeExpiredPasswordRequest;

public interface ChangeExpiredPasswordMusician {

    void execute(String dni, MusicianChangeExpiredPasswordRequest musicianChangeExpiredPasswordRequest);

}
