package com.feyconsuelo.application.service.utils;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Base64;

@Component
@RequiredArgsConstructor
@Slf4j
public class StringService {


    public String nvl(final String value) {
        return org.apache.commons.lang3.StringUtils.isEmpty(value) ? "" : value;
    }

    public String toUpperCase(final String value) {
        return this.nvl(value).toUpperCase();
    }

    public String convertToBase64(final byte[] data) {
        return Base64.getEncoder().encodeToString(data);
    }

}
