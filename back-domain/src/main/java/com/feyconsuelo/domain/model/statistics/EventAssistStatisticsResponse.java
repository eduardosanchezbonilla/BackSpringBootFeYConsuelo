package com.feyconsuelo.domain.model.statistics;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class EventAssistStatisticsResponse {
    private Double averageAssitsNumber;
    private Integer maxAssitsNumber;
    private LocalDate maxDateAssitsNumber;
    private Integer minAssitsNumber;
    private LocalDate minDateAssitsNumber;
}
