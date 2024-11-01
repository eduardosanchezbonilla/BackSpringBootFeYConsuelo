package com.feyconsuelo.domain.usecase.userpartiturerequest;

import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureRequest;

public interface MarkReadUnreadRequestPartiture {

    void execute(final UserRequestPartitureRequest userRequestPartitureRequest);

}
