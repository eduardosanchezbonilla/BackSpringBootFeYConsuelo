package com.feyconsuelo.domain.usecase.musician;

import com.feyconsuelo.domain.model.musician.MusicianGroupByVoiceRequest;
import com.feyconsuelo.domain.model.musician.MusicianGroupByVoiceResponse;

import java.util.List;

public interface GetMusiciansGroupByVoice {

    List<MusicianGroupByVoiceResponse> execute(final MusicianGroupByVoiceRequest musicianGroupByVoiceRequest);

}
