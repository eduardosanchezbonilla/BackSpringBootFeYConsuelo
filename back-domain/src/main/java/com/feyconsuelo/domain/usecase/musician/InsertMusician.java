package com.feyconsuelo.domain.usecase.musician;

import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.domain.model.musician.MusicianResponse;

public interface InsertMusician {

    MusicianResponse execute(MusicianRequest musicianRequest);

}
