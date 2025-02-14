package com.feyconsuelo.domain.model.musicianmarchsolo;

import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class MusicianMarchSoloResponse {

    private final String marchName;

    private final List<MusicianSoloResponse> solos;

}
