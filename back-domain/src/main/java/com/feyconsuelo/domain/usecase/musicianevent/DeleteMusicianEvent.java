package com.feyconsuelo.domain.usecase.musicianevent;

import com.feyconsuelo.domain.model.event.EventTypeEnum;

public interface DeleteMusicianEvent {

    void execute(Long musicianId, EventTypeEnum eventType, Long eventId);

}
