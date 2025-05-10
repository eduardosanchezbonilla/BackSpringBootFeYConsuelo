package com.feyconsuelo.application.usecase.survey;

import com.feyconsuelo.application.service.survey.SurveyService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.model.survey.Survey;
import com.feyconsuelo.domain.usecase.survey.InsertSurvey;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertSurveyImpl implements InsertSurvey {

    private final SurveyService surveyService;

    private final ResizeImageImpl resizeImageService;

    @Value("${default-images.survey}")
    private String defaultSurveyImage;

    @Override
    public void execute(final Survey survey) {
        // si estan enviando imagen, debemos redimensionarla
        if (StringUtils.isNotEmpty(survey.getImage()) && !survey.getImage().equals(this.defaultSurveyImage)) {
            survey.setImageThumbnail(this.resizeImageService.resizeImage(survey.getImage()));
        }
        this.surveyService.insert(survey);
    }

}
