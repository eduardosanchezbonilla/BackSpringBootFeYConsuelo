package com.feyconsuelo.application.usecase.musicianrehearsal;

import com.feyconsuelo.application.service.musicianrehearsal.MusicianRehearsalService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteMusicianRehearsalImpl {

    private final MusicianRehearsalService musicianRehearsalService;

    public void execute(final Long musicianId, final Long eventId) {
        this.musicianRehearsalService.logicalDelete(musicianId, eventId);
    }

}
