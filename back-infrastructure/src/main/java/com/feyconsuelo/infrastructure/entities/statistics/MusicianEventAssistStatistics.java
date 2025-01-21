package com.feyconsuelo.infrastructure.entities.statistics;

public interface MusicianEventAssistStatistics {
    Integer getMusicianHistoricTotalNumberRehearsalEvents();

    Integer getMusicianHistoricAssistNumberRehearsalEvents();

    Integer getMusicianHistoricTotalNumberPerformanceEvents();

    Integer getMusicianHistoricAssistNumberPerformanceEvents();

    Integer getMusicianHistoricTotalNumberEvents();

    Integer getMusicianHistoricAssistNumberEvents();

    Integer getMusicianFromDateTotalNumberRehearsalEvents();

    Integer getMusicianFromDateAssistNumberRehearsalEvents();

    Integer getMusicianFromDateTotalNumberPerformanceEvents();

    Integer getMusicianFromDateAssistNumberPerformanceEvents();

    Integer getMusicianFromDateTotalNumberEvents();

    Integer getMusicianFromDateAssistNumberEvents();

    Integer getMusicianBetweenDatesTotalNumberRehearsalEvents();

    Integer getMusicianBetweenDatesAssistNumberRehearsalEvents();

    Integer getMusicianBetweenDatesTotalNumberPerformanceEvents();

    Integer getMusicianBetweenDatesAssistNumberPerformanceEvents();

    Integer getMusicianBetweenDatesTotalNumberEvents();

    Integer getMusicianBetweenDatesAssistNumberEvents();

}
