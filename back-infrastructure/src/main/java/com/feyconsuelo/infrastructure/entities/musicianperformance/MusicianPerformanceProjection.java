package com.feyconsuelo.infrastructure.entities.musicianperformance;

import com.feyconsuelo.infrastructure.entities.performance.PerformanceEntity;

public interface MusicianPerformanceProjection {
    Long getPerformanceId();

    Long getMusicianId();

    Integer getFormationPositionX();

    Integer getFormationPositionY();

    PerformanceEntity getPerformance();

}
