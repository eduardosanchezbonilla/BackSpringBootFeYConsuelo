package com.feyconsuelo.infrastructure.entities.survey;

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
@Table(schema = "feyconsuelo", name = "survey")
public class SurveyEntity implements Serializable {
    @Serial
    private static final long serialVersionUID = 2438351663350375592L;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "type", nullable = false)
    private String type;

    @Column(name = "description")
    private String description;

    @Column(name = "question", nullable = false)
    private String question;

    @Column(name = "is_public", nullable = false)
    private Boolean isPublic;

    @Column(name = "is_open", nullable = false)
    private Boolean isOpen;

    @Column(name = "is_finished", nullable = false)
    private Boolean isFinished;

    @Column(name = "image")
    private String image;

    @Column(name = "image_thumbnail")
    private String imageThumbnail;

    @Column(name = "update_user", nullable = false)
    private String modifiedUserSurvey;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "update_date", nullable = false)
    @LastModifiedDate
    private LocalDateTime modifiedDateSurvey;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "delete_date")
    private LocalDateTime deleteDateSurvey;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "creation_date", nullable = false, updatable = false)
    @CreatedDate
    private LocalDateTime createdDateSurvey;


}
