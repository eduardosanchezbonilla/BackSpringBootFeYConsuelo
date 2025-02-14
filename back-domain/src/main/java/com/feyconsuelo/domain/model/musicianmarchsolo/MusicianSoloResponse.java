package com.feyconsuelo.domain.model.musicianmarchsolo;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class MusicianSoloResponse {

    private String soloName;

    private Integer soloOrder;

    private Integer mainSoloistOrder;

    private Integer minMainSoloistOrder;

    private Integer maxMainSoloistOrder;

    private Integer secondarySoloistOrder;

    private Integer minSecondarySoloistOrder;
    
    private Integer maxSecondarySoloistOrder;

}
