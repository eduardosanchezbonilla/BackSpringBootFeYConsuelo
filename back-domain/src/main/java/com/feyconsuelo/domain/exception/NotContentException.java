package com.feyconsuelo.domain.exception;

import lombok.extern.slf4j.Slf4j;

import java.io.Serial;

@Slf4j
public class NotContentException extends RuntimeException {

    private static final String ERROR_MESSAGE = "Exception: {}";
    @Serial
    private static final long serialVersionUID = 2016997697056709110L;

    public NotContentException() {
        super();
    }

    public NotContentException(final String message, final Throwable cause) {
        super(message, cause);
        log.error("Message: {}", message, cause);
        log.error(ERROR_MESSAGE, message, cause);
    }

    public NotContentException(final String message) {
        super(message);
        log.error(ERROR_MESSAGE, message);
    }

    public NotContentException(final Throwable cause) {
        super(cause);
        log.error(ERROR_MESSAGE, cause.getMessage(), cause);
    }
}
