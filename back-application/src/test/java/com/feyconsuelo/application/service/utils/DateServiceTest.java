package com.feyconsuelo.application.service.utils;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

@ExtendWith(MockitoExtension.class)
class DateServiceTest {

    @InjectMocks
    private DateService dateService;

    @Test
    void stringToDate() {
        // setup
        final String value = "2024-10-03T15:05:00+02:00";

        // call
        final LocalDateTime result = this.dateService.stringToDate(value, DateTimeFormatter.ISO_DATE_TIME);

        // assert
        Assertions.assertNotNull(result);

    }
}