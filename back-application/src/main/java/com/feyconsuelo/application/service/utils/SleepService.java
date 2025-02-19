package com.feyconsuelo.application.service.utils;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

@Component
@RequiredArgsConstructor
@Slf4j
public class SleepService {

    public void sleep(final long miliseconds) {
        try {
            TimeUnit.MILLISECONDS.sleep(miliseconds);
        } catch (final InterruptedException e) {
            Thread.currentThread().interrupt();
            log.error("Error al dormir el hilo: {}", e.getMessage(), e);
        }
    }

}
