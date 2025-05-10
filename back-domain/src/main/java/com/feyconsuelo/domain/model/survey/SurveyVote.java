package com.feyconsuelo.domain.model.survey;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class SurveyVote {
    private String username;
    private Long id;
    private List<SurveyOptionVote> options;

}
