package com.feyconsuelo.domain.model.repertoiremarchtype;

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
public class RepertoireMarchTypeResponse {

    private String name;

    private Long id;

    private String image;
    
    private Integer order;

    private LocalDateTime deleteDate;

}
