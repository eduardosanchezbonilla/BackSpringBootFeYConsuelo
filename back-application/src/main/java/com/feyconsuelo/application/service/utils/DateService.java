package com.feyconsuelo.application.service.utils;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

@Component
@RequiredArgsConstructor
@Slf4j
public class DateService {

    public LocalDateTime stringToDate(final String value, final DateTimeFormatter formatter) {
        if (StringUtils.isEmpty(value)) {
            return null;
        } else {
            return LocalDateTime.parse(value.replace("Z", ""), formatter);
        }
    }

    public String dateToString(final LocalDateTime date, final DateTimeFormatter formatter) {
        if (date == null) {
            return null;
        } else {
            return date.format(formatter);
        }
    }
}
