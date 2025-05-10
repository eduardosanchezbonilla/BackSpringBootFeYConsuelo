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
public class Survey {
    private Long id;
    private String name;
    private String type;
    private String description;
    private String question;
    private Boolean isPublic;
    private Boolean isOpen;
    private Boolean isFinished;
    private String image;
    private String imageThumbnail;
    private String createdTime;
    private List<SurveyOption> options;
    private List<SurveyOptionVote> userOptionsVote;
    private Integer numberUsersVote;
}
