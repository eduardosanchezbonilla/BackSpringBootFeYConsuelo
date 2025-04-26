package com.feyconsuelo.infrastructure.entities.performance;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

public interface PerformanceMusiciansProjection {
    Long getPerformanceId();

    LocalDate getDate();

    LocalDateTime getEndTime();

    LocalDateTime getStartTime();

    String getDescription();

    String getTitle();

    List<Integer> getVoiceIdList();

    String getPerformanceType();

    String getLocation();

    String getProvince();

    String getMunicipality();

    String getImageThumbnail();

    Boolean getEventPublic();

    Boolean getDisplacementBus();

    Boolean getRepertoirePublic();

    Boolean getBusData();

    Boolean getCrossheadPublic();

    LocalDateTime getBusTime();

    String getRoute();

    String getBusLocation();

    Double getCurrentLongitude();

    Double getCurrentLatitude();

    Double getDuration();

    String getCurrentMarch();

    String getGoogleId();

    Double getKilometers();

    String getMusicians();

    String getFakeMusicians();
}
