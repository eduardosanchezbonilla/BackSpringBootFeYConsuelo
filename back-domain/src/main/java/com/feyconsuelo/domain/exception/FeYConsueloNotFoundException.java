package com.feyconsuelo.domain.exception;

import java.io.Serial;

public class FeYConsueloNotFoundException extends RuntimeException {

    @Serial
    private static final long serialVersionUID = 9061479496379380949L;

    public FeYConsueloNotFoundException(final String message) {
        super(message);
    }


}
