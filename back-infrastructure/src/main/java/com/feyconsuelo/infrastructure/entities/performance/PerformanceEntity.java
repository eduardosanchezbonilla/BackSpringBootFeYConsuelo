package com.feyconsuelo.infrastructure.entities.performance;

import io.hypersistence.utils.hibernate.type.array.ListArrayType;
import io.hypersistence.utils.hibernate.type.json.JsonBinaryType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.Type;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EntityListeners(AuditingEntityListener.class)
@Table(schema = "feyconsuelo", name = "performance")
public class PerformanceEntity implements Serializable {
    @Serial
    private static final long serialVersionUID = 2438351663350375592L;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "date", nullable = false)
    private LocalDate date;

    @Column(name = "end_time", nullable = false)
    private LocalDateTime endTime;

    @Column(name = "start_time", nullable = false)
    private LocalDateTime startTime;

    @Column(name = "title")
    private String title;

    @Column(name = "description")
    private String description;

    @Column(name = "performance_type")
    private String performanceType;

    @Type(ListArrayType.class)
    @Column(name = "voice_id_list")
    private List<Integer> voiceIdList;

    @Column(name = "location")
    private String location;

    @Column(name = "municipality")
    private String municipality;

    @Column(name = "image")
    private String image;

    @Column(name = "image_thumbnail")
    private String imageThumbnail;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "creation_date", nullable = false, updatable = false)
    @CreatedDate
    private LocalDateTime createdDate;

    @Column(name = "update_user", nullable = false)
    private String modifiedUser;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "delete_date")
    private LocalDateTime deleteDate;

    @Column(name = "province")
    private String province;

    @Column(name = "bus")
    private Boolean bus;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "update_date", nullable = false)
    @LastModifiedDate
    private LocalDateTime modifiedDate;

    @Type(JsonBinaryType.class)
    @Column(name = "route", columnDefinition = "jsonb")
    private String route;

    @Column(name = "current_latitude")
    private Double currentLat;

    @Column(name = "current_longitude")
    private Double currentLng;

    @Column(name = "current_march")
    private String currentMarch;

    @Column(name = "event_public")
    private Boolean eventPublic;

    @Column(name = "repertoire_public")
    private Boolean repertoirePublic;

    @Column(name = "crosshead_public")
    private Boolean crossheadPublic;

    @Column(name = "bus_data")
    private Boolean busData;

    @Column(name = "bus_time")
    private LocalDateTime busTime;

    @Column(name = "bus_location")
    private String busLocation;

    @Column(name = "duration")
    private Double duration;

    @Column(name = "kilometers")
    private Double kilometers;

    @Column(name = "google_id")
    private String googleId;
}
