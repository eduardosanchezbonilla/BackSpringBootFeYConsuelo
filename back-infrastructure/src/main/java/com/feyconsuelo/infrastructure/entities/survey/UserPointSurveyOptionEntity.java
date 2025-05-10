package com.feyconsuelo.infrastructure.entities.survey;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
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
@Table(schema = "feyconsuelo", name = "user_point_survey_option")
public class UserPointSurveyOptionEntity implements Serializable {
    @Serial
    private static final long serialVersionUID = 2438351663350375592L;

    @EmbeddedId
    private UserPointSurveyOptionPK id;

    @Column(name = "points")
    private Integer points;

    @Column(name = "update_user", nullable = false)
    private String modifiedUserSurveyVoteOption;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "update_date", nullable = false)
    @LastModifiedDate
    private LocalDateTime modifiedDateSurveyVoteOption;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "delete_date")
    private LocalDateTime deleteDateSurveyVoteOption;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "creation_date", nullable = false, updatable = false)
    @CreatedDate
    private LocalDateTime createdDateSurveyVoteOption;


}
