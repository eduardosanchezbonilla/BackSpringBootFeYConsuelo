package com.feyconsuelo.infrastructure.entities.musicianrehearsal;

import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalEntity;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

public interface MusicianRehearsalProjection {
    Long getRehearsalId();

    LocalDate getDate();

    LocalDateTime getStartTime();

    LocalDateTime getEndTime();

    String getDescription();

    List<Integer> getVoiceIdList();

    String getLocation();

    String getMunicipality();

    String getProvince();

    Long getMusicianId();

    Integer getFormationPositionX();

    Integer getFormationPositionY();

    Double getDuration();

    RehearsalEntity getRehearsal();

}