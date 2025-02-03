package com.feyconsuelo.domain.model.repertoire;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class RepertoireMarchSolo {

    private Long id;

    private String name;

    private Integer order;

    private List<RepertoireMarchSoloist> mainSoloists;

    private List<RepertoireMarchSoloist> secondarySoloists;


}
