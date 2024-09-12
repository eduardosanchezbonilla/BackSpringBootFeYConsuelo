package com.feyconsuelo.domain.exception;

import lombok.extern.slf4j.Slf4j;

import java.io.Serial;

@Slf4j
public class FeYConsueloException extends RuntimeException {

    private static final String ERROR_MESSAGE = "Exception: {}";
    @Serial
    private static final long serialVersionUID = 1819216014951894478L;

    public FeYConsueloException() {
        super();
    }

    public FeYConsueloException(final String message, final Throwable cause) {
        super(message, cause);
        log.error("Message: {}", message, cause);
        log.error(ERROR_MESSAGE, message, cause);
    }

    public FeYConsueloException(final String message) {
        super(message);
        log.error(ERROR_MESSAGE, message);
    }

    public FeYConsueloException(final Throwable cause) {
        super(cause);
        log.error(ERROR_MESSAGE, cause.getMessage(), cause);
    }
}
