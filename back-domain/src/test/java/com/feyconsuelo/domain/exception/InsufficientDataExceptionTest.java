package com.feyconsuelo.domain.exception;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class InsufficientDataExceptionTest {

    @Test
    void dafaultConstructor() {

        // setup

        // call
        final InsufficientDataException exception = new InsufficientDataException();

        // assertions
        assertThat(exception).isNotNull();
    }

    @Test
    void messageConstructor() {

        // setup
        final String message = "message";

        // call
        final InsufficientDataException exception = new InsufficientDataException(message);

        // assertions
        assertThat(exception).isNotNull();
        assertThat(message).isEqualTo(exception.getMessage());
    }

    @Test
    void causeConstructor() {

        // setup
        final Throwable cause = new Throwable();

        // call
        final InsufficientDataException exception = new InsufficientDataException(cause);

        // assertions
        assertThat(exception).isNotNull();
        assertThat(cause).isEqualTo(exception.getCause());
    }

    @Test
    void allParameterConstructor() {

        // setup
        final String message = "message";
        final Throwable cause = new Throwable();

        // call
        final InsufficientDataException exception = new InsufficientDataException(message, cause);

        // assertions
        assertThat(exception).isNotNull();
        assertThat(message).isEqualTo(exception.getMessage());
        assertThat(cause).isEqualTo(exception.getCause());
    }
}