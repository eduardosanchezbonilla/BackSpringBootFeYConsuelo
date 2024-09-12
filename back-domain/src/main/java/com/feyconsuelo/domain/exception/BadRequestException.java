package com.feyconsuelo.domain.exception;

import lombok.extern.slf4j.Slf4j;

import java.io.Serial;

@Slf4j
public class BadRequestException extends RuntimeException {

    private static final String ERROR_MESSAGE = "Exception: {}";
    @Serial
    private static final long serialVersionUID = 415344895640459845L;

    public BadRequestException() {
        super();
    }

    public BadRequestException(final String message, final Throwable cause) {
        super(message, cause);
        log.error("Message: {}", message, cause);
        log.error(ERROR_MESSAGE, message, cause);
    }

    public BadRequestException(final String message) {
        super(message);
        log.error(ERROR_MESSAGE, message);
    }

    public BadRequestException(final Throwable cause) {
        super(cause);
        log.error(ERROR_MESSAGE, cause.getMessage(), cause);
    }
}
