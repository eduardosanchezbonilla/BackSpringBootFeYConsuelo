package com.feyconsuelo.domain.model.inventory;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class InventoryResponse {

    private Long id;

    private String name;

    private Integer order;

    private Integer units;

    private String image;

    private LocalDateTime deleteDate;

    private Integer musicianWithElement;
}
