package com.feyconsuelo.apirest.handler;

import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.exception.NotAuthorizedException;
import com.feyconsuelo.domain.exception.NotContentException;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.exception.PasswordExpiredException;
import com.feyconsuelo.openapi.model.ErrorDto;
import jakarta.validation.ConstraintViolationException;
import lombok.extern.slf4j.Slf4j;
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

    @ExceptionHandler(PasswordExpiredException.class)
    public ResponseEntity<ErrorDto> passwordExpiredException(final PasswordExpiredException ex) {
        return new ResponseEntity<>(
                ErrorDto.builder()
                        .message(ex.getMessage())
                        .build(),
                HttpStatus.FORBIDDEN
        );
    }

    @ExceptionHandler(NotAuthorizedException.class)
    public ResponseEntity<ErrorDto> notAuthorizedException(final NotAuthorizedException ex) {
        return new ResponseEntity<>(
                ErrorDto.builder()
                        .message(ex.getMessage())
                        .build(),
                HttpStatus.UNAUTHORIZED
        );
    }

    @ExceptionHandler({NotContentException.class})
    public ResponseEntity<ErrorDto> notContentException(final NotContentException ex) {
        return new ResponseEntity<>(
                ErrorDto.builder()
                        .message(ex.getMessage())
                        .build(),
                HttpStatus.NO_CONTENT
        );
    }

    @ExceptionHandler({NotFoundException.class})
    public ResponseEntity<ErrorDto> notFountException(final NotFoundException ex) {
        return new ResponseEntity<>(
                ErrorDto.builder()
                        .message(ex.getMessage())
                        .build(),
                HttpStatus.NOT_FOUND
        );
    }

    @ExceptionHandler({
            BadRequestException.class,
            MissingServletRequestParameterException.class,
            MethodArgumentNotValidException.class,
            MethodArgumentTypeMismatchException.class,
            HttpMessageNotReadableException.class,
            ConstraintViolationException.class,
            HttpRequestMethodNotSupportedException.class,
            HttpMessageConversionException.class
    })
    public ResponseEntity<ErrorDto> badRequestException(final Exception ex) {
        return new ResponseEntity<>(
                ErrorDto.builder()
                        .message(ex.getMessage())
                        .build(),
                HttpStatus.BAD_REQUEST
        );
    }

    @ExceptionHandler(FeYConsueloException.class)
    public ResponseEntity<ErrorDto> feYConsueloException(final FeYConsueloException ex) {
        return new ResponseEntity<>(
                ErrorDto.builder()
                        .message(ex.getMessage())
                        .build(),
                HttpStatus.INTERNAL_SERVER_ERROR
        );
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorDto> handleException(final Exception ex) {
        log.error("Internal error {}", ex.getMessage(), ex);
        return new ResponseEntity<>(
                ErrorDto.builder()
                        .message(INTERNAL_ERROR)
                        .build(),
                HttpStatus.INTERNAL_SERVER_ERROR
        );
    }

}
