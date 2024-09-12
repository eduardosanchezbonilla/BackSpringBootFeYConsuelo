package com.feyconsuelo.domain.exception;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class BadRequestExceptionTest {

    @Test
    void dafaultConstructor() {

        // setup

        // call
        final BadRequestException exception = new BadRequestException();

        // assertions
        assertThat(exception).isNotNull();
    }

    @Test
    void messageConstructor() {

        // setup
        final String message = "message";

        // call
        final BadRequestException exception = new BadRequestException(message);

        // assertions
        assertThat(exception).isNotNull();
        assertThat(message).isEqualTo(exception.getMessage());
    }

    @Test
    void causeConstructor() {

        // setup
        final Throwable cause = new Throwable();

        // call
        final BadRequestException exception = new BadRequestException(cause);

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
        final BadRequestException exception = new BadRequestException(message, cause);

        // assertions
        assertThat(exception).isNotNull();
        assertThat(message).isEqualTo(exception.getMessage());
        assertThat(cause).isEqualTo(exception.getCause());
    }
}