package com.feyconsuelo.infrastructure.converter.survey;

import com.feyconsuelo.domain.model.survey.Survey;
import com.feyconsuelo.domain.model.survey.SurveyOption;
import com.feyconsuelo.infrastructure.entities.survey.SurveyEntity;
import com.feyconsuelo.infrastructure.entities.survey.SurveyOptionEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class SurveyToSurveyEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    @Value("${default-images.survey}")
    private String defaultSurveyImage;

    private String getSurveyImage(final String image) {
        if (StringUtils.isEmpty(image)) {
            return image;
        } else {
            if (image.equals(this.defaultSurveyImage)) {
                return null;
            } else {
                return image;
            }
        }
    }

    public SurveyEntity convert(final Survey survey) {
        return SurveyEntity.builder()
                .name(survey.getName())
                .type(survey.getType())
                .description(survey.getDescription())
                .question(survey.getQuestion())
                .isPublic(survey.getIsPublic() == null ? Boolean.FALSE : survey.getIsPublic())
                .isOpen(survey.getIsOpen() == null ? Boolean.FALSE : survey.getIsOpen())
                .isFinished(survey.getIsFinished() == null ? Boolean.FALSE : survey.getIsFinished())
                .image(this.getSurveyImage(survey.getImage()))
                .imageThumbnail(this.getSurveyImage(survey.getImageThumbnail()))
                .modifiedUserSurvey(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public SurveyEntity updateEntity(final SurveyEntity surveyEntity,
                                     final Survey survey) {
        surveyEntity.setName(survey.getName());
        surveyEntity.setType(survey.getType());
        surveyEntity.setDescription(survey.getDescription());
        surveyEntity.setQuestion(survey.getQuestion());
        surveyEntity.setIsPublic(survey.getIsPublic() == null ? Boolean.FALSE : survey.getIsPublic());
        surveyEntity.setIsOpen(survey.getIsOpen() == null ? Boolean.FALSE : survey.getIsOpen());
        surveyEntity.setIsFinished(survey.getIsFinished() == null ? Boolean.FALSE : survey.getIsFinished());
        surveyEntity.setImage(this.getSurveyImage(survey.getImage()));
        surveyEntity.setImageThumbnail(this.getSurveyImage(survey.getImageThumbnail()));
        surveyEntity.setModifiedUserSurvey(this.tokenInfoExtractorService.getUsername());
        return surveyEntity;
    }

    public List<SurveyOptionEntity> convertOptions(final Long surveyId, final List<SurveyOption> options) {
        if (CollectionUtils.isEmpty(options)) {
            return List.of();
        } else {
            return options.stream()
                    .map(option -> SurveyOptionEntity.builder()
                            .id(option.getId())
                            .surveyId(surveyId)
                            .name(option.getName())
                            .description(option.getDescription())
                            .youtubeId(option.getYoutubeId())
                            .order(option.getOrder())
                            .modifiedUserSurveyOption(this.tokenInfoExtractorService.getUsername())
                            .build()
                    )
                    .toList();
        }
    }

}
