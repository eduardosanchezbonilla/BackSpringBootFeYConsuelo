package com.feyconsuelo.infrastructure.entities.survey;

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
public class UserPointSurveyOptionPK implements Serializable {
    private static final long serialVersionUID = 1L;

    @Column(name = "username")
    private String username;

    @Column(name = "survey_id")
    private Long surveyId;

    @Column(name = "option_id")
    private Long optionId;

}
