package com.feyconsuelo.domain.model.repertoirecategory;

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
public class RepertoireCategoryRequest {

    private String name;

    private Integer order;

    private Boolean current;

    private String image;

}
