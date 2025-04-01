package com.feyconsuelo.domain.model.partiture;

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
public class PartitureResponse {

    private String name;

    private Long id;

    private String content;

    private LocalDateTime deleteDate;

    private String googleId;

    private String mimeType;

}
