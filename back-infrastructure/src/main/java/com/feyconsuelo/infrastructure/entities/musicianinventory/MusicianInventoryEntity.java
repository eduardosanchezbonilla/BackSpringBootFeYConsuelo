package com.feyconsuelo.infrastructure.entities.musicianinventory;

import com.feyconsuelo.infrastructure.entities.inventory.InventoryEntity;
import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
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
@Table(schema = "feyconsuelo", name = "musician_inventory")
public class MusicianInventoryEntity implements Serializable {
    @Serial
    private static final long serialVersionUID = 2438351663350375592L;

    @EmbeddedId
    private MusicianInventoryPK id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "musician_id", referencedColumnName = "id", insertable = false, updatable = false)
    private MusicianEntity musician;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "inventory_id", referencedColumnName = "id", insertable = false, updatable = false)
    private InventoryEntity inventory;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "update_date", nullable = false)
    @LastModifiedDate
    private LocalDateTime updateDate;

    @Column(name = "update_user", nullable = false)
    private String updateUser;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "creation_date", nullable = false, updatable = false)
    @CreatedDate
    private LocalDateTime createdDate;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "delete_date")
    private LocalDateTime deleteDate;

}
