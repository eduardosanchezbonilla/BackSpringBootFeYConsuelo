package com.feyconsuelo.infrastructure.entities.repertoire;

import java.time.LocalDateTime;

public interface RepertoireMarchProjection {

    Long getId();

    Long getCategoryId();

    String getCategory();

    Long getTypeId();

    String getType();

    String getName();

    String getAuthor();

    String getDescription();

    String getYoutubeId();

    LocalDateTime getDeleteDate();

    String getSolos();
}
