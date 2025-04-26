package com.feyconsuelo.infrastructure.entities.musicianperformance;

import com.feyconsuelo.infrastructure.entities.performance.PerformanceEntity;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

public interface MusicianPerformanceProjection {
    Long getPerformanceId();

    LocalDate getDate();

    LocalDateTime getStartTime();

    LocalDateTime getEndTime();

    String getTitle();

    String getDescription();

    String getPerformanceType();

    List<Integer> getVoiceIdList();

    String getLocation();

    String getMunicipality();

    String getProvince();

    String getImageThumbnail();

    Boolean getDisplacementBus();

    Boolean getEventPublic();

    Boolean getRepertoirePublic();

    Boolean getCrossheadPublic();

    Boolean getBusData();

    LocalDateTime getBusTime();

    String getBusLocation();

    String getRoute();

    Double getCurrentLatitude();

    Double getCurrentLongitude();

    String getCurrentMarch();

    Double getDuration();

    Double getKilometers();

    String getGoogleId();

    Long getMusicianId();

    Integer getFormationPositionX();

    Integer getFormationPositionY();

    Boolean getMusicianBus();

    PerformanceEntity getPerformance();

    String getMusicianDni();

    String getMusicianName();

    String getMusicianSurname();

    String getMusicianDirection();

    String getMusicianMunicipality();

    String getMusicianProvince();

    String getMusicianEmail();

    Long getVoiceId();

    Integer getVoiceOrder();

    String getVoiceName();

    String getMusicianImage();

    LocalDateTime getMusicianDeleteDate();

    LocalDateTime getMusicianBirthDate();

    LocalDateTime getMusicianRegistrationDate();

    LocalDateTime getMusicianUnregistrationDate();

    Boolean getMusicianUnregistred();

    LocalDateTime getMusicianDateLastNotificationNonAssistsStreakRehearsals();

    String getMusicianInventoryObservations();

    String getMusicianPhoneNumber();

    String getMusicianObservations();
}
