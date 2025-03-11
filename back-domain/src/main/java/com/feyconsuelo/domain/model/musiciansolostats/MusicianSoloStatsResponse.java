package com.feyconsuelo.domain.model.musiciansolostats;

import lombok.Builder;
import lombok.Value;

@Value
@Builder
public class MusicianSoloStatsResponse {

    Long id;

    String name;

    String surname;

    Integer totalSolos;

    Integer totalMainSolos;

    Integer totalSecondarySolos;

}
