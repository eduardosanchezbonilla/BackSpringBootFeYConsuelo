package com.feyconsuelo.infrastructure.entities.musicianperformance;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.io.Serializable;

@Embeddable
@Data
@SuperBuilder
@NoArgsConstructor
public class MusicianPerformancePK implements Serializable {
    private static final long serialVersionUID = 1L;

    @Column(name = "musician_id")
    private Long musicianId;

    @Column(name = "performance_id")
    private Long performanceId;

}
