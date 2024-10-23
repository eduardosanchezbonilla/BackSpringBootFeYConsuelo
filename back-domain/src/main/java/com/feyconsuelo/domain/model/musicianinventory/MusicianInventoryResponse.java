package com.feyconsuelo.domain.model.musicianinventory;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class MusicianInventoryResponse {

    private Long musicianId;

    private Long inventoryId;

    private String inventoryName;

    private String inventoryImage;

    private Integer inventoryOrder;

    private Boolean assigned;

    private LocalDateTime deleteDate;

}
