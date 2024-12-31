package com.feyconsuelo.infrastructure.entities.inventory;

import java.time.LocalDateTime;

public interface Inventory {
    Long getId();

    Integer getOrder();

    Integer getUnits();

    String getName();

    String getImage();

    Integer getMusicians();

    LocalDateTime getDeleteDate();

}
