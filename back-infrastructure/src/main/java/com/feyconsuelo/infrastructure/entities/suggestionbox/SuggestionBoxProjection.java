package com.feyconsuelo.infrastructure.entities.suggestionbox;

import java.time.LocalDateTime;


public interface SuggestionBoxProjection {

    Long getId();

    String getSuggestion();

    Boolean getReaded();

    LocalDateTime getDeleteDate();

    String getUsername();

    String getDni();

    String getName();

    String getSurname();

    String getDirection();

    String getMunicipality();

    String getProvince();

    String getEmail();

    String getDescription();

    String getImage();

    String getPhoneNumber();
}
