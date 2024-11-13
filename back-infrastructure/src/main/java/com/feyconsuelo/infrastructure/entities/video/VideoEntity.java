package com.feyconsuelo.infrastructure.entities.video;

import com.feyconsuelo.infrastructure.entities.videocategory.VideoCategoryEntity;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
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
@Table(schema = "feyconsuelo", name = "video")
public class VideoEntity implements Serializable {
    @Serial
    private static final long serialVersionUID = 2438351663350375592L;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "youtube_id", nullable = false)
    private String youtubeId;

    @OneToOne(cascade = {CascadeType.PERSIST, CascadeType.MERGE})
    @JoinColumn(name = "video_category_id", referencedColumnName = "id")
    private VideoCategoryEntity videoCategory;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "description")
    private String description;

    @Column(name = "video_order", nullable = false)
    private Integer order;

    @Column(name = "update_user", nullable = false)
    private String modifiedUser;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "update_date", nullable = false)
    @LastModifiedDate
    private LocalDateTime videoModifiedDate;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "delete_date")
    private LocalDateTime deleteDate;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "creation_date", nullable = false, updatable = false)
    @CreatedDate
    private LocalDateTime createdDate;


}
