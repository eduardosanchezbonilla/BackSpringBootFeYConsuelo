package com.feyconsuelo.domain.model.repertoire;

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
public class RepertoireMarchGroupByTypeRequest {

    private Long categoryId;

    private String name;

    private Boolean current;

}
