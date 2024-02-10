package com.feyconsuelo.apirest.handler;

import com.feyconsuelo.domain.exception.FeYConsueloNotFoundException;
import com.feyconsuelo.openapi.model.ErrorDTO;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.exception.ConstraintViolationException;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageConversionException;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;

@Slf4j
@RestControllerAdvice
public final class GlobalControllerExceptionHandler {

    private static final String INTERNAL_ERROR = "Internal error";

    @ExceptionHandler(DataIntegrityViolationException.class)
    public ResponseEntity<ErrorDTO> handleException(final DataIntegrityViolationException ex) {
        log.error("Internal error {}", ex.getMessage(), ex);
        String message = INTERNAL_ERROR;
        if (ex.getCause() instanceof final ConstraintViolationException cv &&
                cv.getSQLException().getMessage().contains("duplicate key")) {
            message = "Duplicate key error";
        }
        return new ResponseEntity<>(
                ErrorDTO.builder().message(message).build(),
                HttpStatus.INTERNAL_SERVER_ERROR
        );
    }

    @ExceptionHandler({
            FeYConsueloNotFoundException.class,
            MissingServletRequestParameterException.class,
            MethodArgumentNotValidException.class,
            MethodArgumentTypeMismatchException.class,
            HttpMessageNotReadableException.class,
            jakarta.validation.ConstraintViolationException.class,
            HttpRequestMethodNotSupportedException.class,
            HttpMessageConversionException.class
    })
    public ResponseEntity<ErrorDTO> badRequestException(final Exception ex) {
        return new ResponseEntity<>(
                ErrorDTO.builder().message(ex.getMessage()).build(),
                HttpStatus.BAD_REQUEST
        );
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorDTO> handleException(final Exception ex) {
        log.error("Internal error {}", ex.getMessage(), ex);
        return new ResponseEntity<>(
                ErrorDTO.builder().message(INTERNAL_ERROR).build(),
                HttpStatus.INTERNAL_SERVER_ERROR
        );
    }

}
