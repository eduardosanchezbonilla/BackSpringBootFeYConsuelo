package com.feyconsuelo.domain.usecase.musicianevent;

import com.feyconsuelo.domain.model.musicianevent.MusicianEventRequest;

public interface InsertMusicianEvent {

    void execute(MusicianEventRequest musicianEventRequest);

}
