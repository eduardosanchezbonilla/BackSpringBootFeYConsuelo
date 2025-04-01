package com.feyconsuelo.infrastructure.entities.musicianrehearsal;

import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalEntity;

public interface MusicianRehearsalProjection {
    Long getRehearsalId();

    Long getMusicianId();

    Integer getFormationPositionX();

    Integer getFormationPositionY();

    RehearsalEntity getRehearsal();

}
