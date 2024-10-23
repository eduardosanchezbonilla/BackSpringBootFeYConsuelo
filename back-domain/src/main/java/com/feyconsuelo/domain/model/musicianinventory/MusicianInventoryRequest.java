package com.feyconsuelo.domain.model.musicianinventory;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class MusicianInventoryRequest {

    private Long musicianId;

    private Long inventoryId;

    private LocalDateTime deletedDate;

}
