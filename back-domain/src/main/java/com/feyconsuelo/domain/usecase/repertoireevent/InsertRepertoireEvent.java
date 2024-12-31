package com.feyconsuelo.domain.usecase.repertoireevent;

import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventRequest;

public interface InsertRepertoireEvent {

    void execute(RepertoireEventRequest repertoireEventRequest);

}
