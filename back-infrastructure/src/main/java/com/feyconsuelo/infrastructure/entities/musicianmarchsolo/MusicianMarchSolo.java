package com.feyconsuelo.infrastructure.entities.musicianmarchsolo;

public interface MusicianMarchSolo {
    String getMarchName();

    String getSoloName();

    Integer getSoloOrder();

    Integer getMainSoloistOrder();

    Integer getMinMainOrder();

    Integer getMaxMainOrder();

    Integer getSecondarySoloistOrder();

    Integer getMinSecondaryOrder();

    Integer getMaxSecondaryOrder();
}
