package com.feyconsuelo.infrastructure.entities.repertoirerehearsal;

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
public class RepertoireRehearsalPK implements Serializable {
    private static final long serialVersionUID = 1L;

    @Column(name = "march_id")
    private Long marchId;

    @Column(name = "rehearsal_id")
    private Long rehearsalId;

}
