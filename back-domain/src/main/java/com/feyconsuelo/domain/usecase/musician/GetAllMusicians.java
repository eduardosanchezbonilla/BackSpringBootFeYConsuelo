package com.feyconsuelo.domain.usecase.musician;

import com.feyconsuelo.domain.model.musician.MusicianResponse;

import java.util.List;

public interface GetAllMusicians {

    List<MusicianResponse> execute(Boolean unregistred);

}
