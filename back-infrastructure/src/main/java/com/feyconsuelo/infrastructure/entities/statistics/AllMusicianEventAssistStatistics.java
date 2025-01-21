package com.feyconsuelo.infrastructure.entities.statistics;

public interface AllMusicianEventAssistStatistics {
    Long getMusicianId();

    String getMusicianName();

    String getMusicianSurname();

    Integer getTotalRehearsal();

    Integer getMusicianAssistsRehearsal();

    Double getMusicianPercentageAssistsRehearsal();

}
