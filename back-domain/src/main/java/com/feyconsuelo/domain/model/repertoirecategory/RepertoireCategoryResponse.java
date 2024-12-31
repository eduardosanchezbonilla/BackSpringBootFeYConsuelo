package com.feyconsuelo.domain.model.repertoirecategory;

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
public class RepertoireCategoryResponse {

    private Long id;

    private String name;

    private Integer order;

    private Boolean current;

    private String image;

    private LocalDateTime deleteDate;

}
