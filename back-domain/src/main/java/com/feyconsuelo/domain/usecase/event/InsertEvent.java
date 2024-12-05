package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventRequest;

public interface InsertEvent {

    void execute(EventRequest eventRequest);

}
