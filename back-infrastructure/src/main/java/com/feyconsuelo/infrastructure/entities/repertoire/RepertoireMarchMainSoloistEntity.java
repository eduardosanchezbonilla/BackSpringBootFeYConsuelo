package com.feyconsuelo.infrastructure.entities.repertoire;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
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
@Table(schema = "feyconsuelo", name = "repertoire_march_main_soloist")
public class RepertoireMarchMainSoloistEntity implements Serializable {
    @Serial
    private static final long serialVersionUID = 2438351663350375592L;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "musician_id", nullable = false)
    private Long musicianId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "solo_id", referencedColumnName = "id", insertable = false, updatable = false)
    private RepertoireMarchSoloEntity solo;

    @Column(name = "solo_id", nullable = false)
    private Integer soloId;

    @Column(name = "soloist_order", nullable = false)
    private Integer order;

    @Column(name = "musician_name", nullable = false)
    private String musicianName;

    @Column(name = "update_user", nullable = false)
    private String modifiedUserMarchMainSoloist;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "update_date", nullable = false)
    @LastModifiedDate
    private LocalDateTime modifiedDateMarchMainSoloist;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "delete_date")
    private LocalDateTime deleteDateMarchMainSoloist;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "creation_date", nullable = false, updatable = false)
    @CreatedDate
    private LocalDateTime createdDateMarchMainSoloist;

}
