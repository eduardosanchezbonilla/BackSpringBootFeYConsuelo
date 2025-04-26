package com.feyconsuelo.infrastructure.entities.rehearsal;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

public interface RehearsalMusiciansProjection {
    Long getRehearsalId();

    LocalDate getDate();

    LocalDateTime getEndTime();

    LocalDateTime getStartTime();

    String getDescription();

    List<Integer> getVoiceIdList();

    String getLocation();

    String getMunicipality();

    String getProvince();

    Double getDuration();

    String getFakeMusicians();

    String getMusicians();
}
