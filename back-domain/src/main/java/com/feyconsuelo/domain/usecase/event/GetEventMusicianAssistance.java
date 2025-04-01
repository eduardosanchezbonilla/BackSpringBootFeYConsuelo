package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventMusicianAssistanceResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;

import java.util.Optional;

public interface GetEventMusicianAssistance {

    Optional<EventMusicianAssistanceResponse> execute(final EventTypeEnum eventType, final Long eventId, final Boolean returnFakeMusicians);

}
