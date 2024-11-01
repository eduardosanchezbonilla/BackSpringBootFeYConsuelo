package com.feyconsuelo.domain.usecase.userpartiturerequest;

import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureRequest;

public interface RequestPartiture {

    void execute(final UserRequestPartitureRequest userRequestPartitureRequest);

}
