package com.feyconsuelo.domain.model.statistics;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class RepertoireMarchEventStatisticResponse {
    private Long categoryId;

    private String categoryName;

    private Long typeId;

    private String typeName;

    private String typeImage;

    private Integer typeOrder;

    private Long marchId;

    private String marchName;

    private String marchAuthor;

    private Integer quantity;

    private Double percentage;

    private Integer maxQuantity;

}
