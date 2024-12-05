package com.feyconsuelo.application.usecase.musicianevent;

import com.feyconsuelo.application.usecase.musicianperformance.InsertMusicianPerformanceImpl;
import com.feyconsuelo.application.usecase.musicianrehearsal.InsertMusicianRehearsalImpl;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventRequest;
import com.feyconsuelo.domain.usecase.musicianevent.InsertMusicianEvent;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertMusicianEventImpl implements InsertMusicianEvent {

    private final InsertMusicianPerformanceImpl insertPerformance;
    private final InsertMusicianRehearsalImpl insertRehearsal;

    @Override
    public void execute(final MusicianEventRequest musicianEventRequest) {
        // dependiendo del tipo de evento insertaremos en Actuaciones o Ensayos
        if (EventTypeEnum.REHEARSAL.equals(musicianEventRequest.getEventType())) {
            this.insertRehearsal.insertMusicianRehearsal(musicianEventRequest);
        } else {
            this.insertPerformance.insertMusicianPerformance(musicianEventRequest);
        }
    }

}
