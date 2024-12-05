package com.feyconsuelo.application.usecase.musicianrehearsal;

import com.feyconsuelo.application.service.musicianrehearsal.MusicianRehearsalService;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertMusicianRehearsalImpl {

    private final MusicianRehearsalService musicianRehearsalService;

    public void insertMusicianRehearsal(final MusicianEventRequest musicianEventRequest) {

        // insertamos
        this.musicianRehearsalService.save(musicianEventRequest);

    }

}
