package com.feyconsuelo.domain.model.statistics;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class AllMusicianEventAssistStatisticsResponse {
    private Long musicianId;

    private String musicianName;

    private String musicianSurname;

    private Integer totalRehearsal;

    private Integer musicianAssistsRehearsal;

    private Double musicianPercentageAssistsRehearsal;
}
