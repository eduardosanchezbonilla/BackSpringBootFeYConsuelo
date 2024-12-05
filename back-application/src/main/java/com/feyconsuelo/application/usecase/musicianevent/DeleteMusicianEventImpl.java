package com.feyconsuelo.application.usecase.musicianevent;

import com.feyconsuelo.application.usecase.musicianperformance.DeleteMusicianPerformanceImpl;
import com.feyconsuelo.application.usecase.musicianrehearsal.DeleteMusicianRehearsalImpl;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.musicianevent.DeleteMusicianEvent;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteMusicianEventImpl implements DeleteMusicianEvent {

    private final DeleteMusicianPerformanceImpl deletePerformance;

    private final DeleteMusicianRehearsalImpl deleteRehearsal;

    @Override
    public void execute(final Long musicianId, final EventTypeEnum type, final Long eventId) {

        if (EventTypeEnum.PERFORMANCE.equals(type)) {
            this.deletePerformance.execute(musicianId, eventId);
        } else {
            this.deleteRehearsal.execute(musicianId, eventId);
        }

    }

}
