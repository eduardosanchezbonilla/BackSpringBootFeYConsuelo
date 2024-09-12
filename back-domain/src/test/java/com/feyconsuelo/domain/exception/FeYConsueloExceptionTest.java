package com.feyconsuelo.domain.exception;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class FeYConsueloExceptionTest {

    @Test
    void dafaultConstructor() {

        // setup

        // call
        final FeYConsueloException exception = new FeYConsueloException();

        // assertions
        assertThat(exception).isNotNull();
    }

    @Test
    void messageConstructor() {

        // setup
        final String message = "message";

        // call
        final FeYConsueloException exception = new FeYConsueloException(message);

        // assertions
        assertThat(exception).isNotNull();
        assertThat(message).isEqualTo(exception.getMessage());
    }

    @Test
    void causeConstructor() {

        // setup
        final Throwable cause = new Throwable();

        // call
        final FeYConsueloException exception = new FeYConsueloException(cause);

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
        final FeYConsueloException exception = new FeYConsueloException(message, cause);

        // assertions
        assertThat(exception).isNotNull();
        assertThat(message).isEqualTo(exception.getMessage());
        assertThat(cause).isEqualTo(exception.getCause());
    }
}