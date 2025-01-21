package com.feyconsuelo.infrastructure.entities.repertoirerehearsal;

import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalEntity;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchEntity;
import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;

@Entity
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EntityListeners(AuditingEntityListener.class)
@Table(schema = "feyconsuelo", name = "repertoire_march_rehearsal")
public class RepertoireRehearsalEntity implements Serializable {
    @Serial
    private static final long serialVersionUID = 2438351663350375592L;

    @EmbeddedId
    private RepertoireRehearsalPK id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "march_id", referencedColumnName = "id", insertable = false, updatable = false)
    private RepertoireMarchEntity march;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "rehearsal_id", referencedColumnName = "id", insertable = false, updatable = false)
    private RehearsalEntity rehearsal;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "update_date", nullable = false)
    @LastModifiedDate
    private LocalDateTime updateDateRR;

    @Column(name = "update_user", nullable = false)
    private String updateUserRR;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "creation_date", nullable = false, updatable = false)
    @CreatedDate
    private LocalDateTime createdDateRR;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "delete_date")
    private LocalDateTime deleteDateRR;

    @Column(name = "march_order", nullable = false)
    private Integer order;

    @Column(name = "march_numbers", nullable = false)
    private Integer numbers;

}
