package com.feyconsuelo.infrastructure.entities.statistics;

import java.time.LocalDate;

public interface EventAssistStatistics {
    Double getAverageAssitsNumber();

    Integer getMaxAssitsNumber();

    LocalDate getDateMaxAssitsNumber();

    Integer getMinAssitsNumber();

    LocalDate getDateMinAssitsNumber();

    Double getAverageAssitsPercentage();

    Integer getMaxAssitsPercentage();

    LocalDate getDateMaxAssitsPercentage();

    Integer getMinAssitsPercentage();

    LocalDate getDateMinAssitsPercentage();

}
