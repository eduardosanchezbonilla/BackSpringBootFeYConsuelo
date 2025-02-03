package com.feyconsuelo.domain.model.repertoire;

import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryResponse;
import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class RepertoireMarchResponse {

    private Long id;

    private Long categoryId;

    private RepertoireCategoryResponse category;

    private Long typeId;

    private RepertoireMarchTypeResponse type;

    private String name;

    private String author;

    private String description;

    private String image;

    private String youtubeId;

    private LocalDateTime deleteDate;

    private Boolean checked;

    private Integer order;

    private Integer numbers;

    private List<RepertoireMarchSolo> repertoireMarchSolos;

}
