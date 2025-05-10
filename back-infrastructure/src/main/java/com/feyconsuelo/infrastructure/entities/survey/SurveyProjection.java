package com.feyconsuelo.infrastructure.entities.survey;

import java.time.LocalDateTime;


public interface SurveyProjection {

    Long getId();

    String getName();

    String getType();

    String getDescription();

    String getQuestion();

    Boolean getIsPublic();

    Boolean getIsOpen();

    Boolean getIsFinished();

    String getImage();

    String getImageThumbnail();

    LocalDateTime getDeleteDate();

    LocalDateTime getCreationDate();

    String getOptions();

    String getUserOptionsVote();

    Integer getNumberUsersVote();

}
