package com.feyconsuelo.domain.exception;

import lombok.extern.slf4j.Slf4j;

import java.io.Serial;

@Slf4j
public class InsufficientDataException extends RuntimeException {

    private static final String ERROR_MESSAGE = "Exception: {}";
    @Serial
    private static final long serialVersionUID = 4158730215206571387L;

    public InsufficientDataException() {
        super();
    }

    public InsufficientDataException(final String message, final Throwable cause) {
        super(message, cause);
        log.error("Message: {}", message, cause);
        log.error(ERROR_MESSAGE, message, cause);
    }

    public InsufficientDataException(final String message) {
        super(message);
        log.error(ERROR_MESSAGE, message);
    }

    public InsufficientDataException(final Throwable cause) {
        super(cause);
        log.error(ERROR_MESSAGE, cause.getMessage(), cause);
    }
}
