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
public class RepertoireMarchRequest {

    private Long categoryId;

    private Long typeId;

    private String name;

    private String author;

    private String description;

    private String image;

    private String youtubeId;

    private List<RepertoireMarchSolo> repertoireMarchSolos;

}
