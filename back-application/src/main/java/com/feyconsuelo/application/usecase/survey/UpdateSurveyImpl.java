package com.feyconsuelo.application.usecase.survey;

import com.feyconsuelo.application.service.survey.SurveyService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.survey.Survey;
import com.feyconsuelo.domain.usecase.survey.UpdateSurvey;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UpdateSurveyImpl implements UpdateSurvey {

    private final SurveyService surveyService;
    private final ResizeImageImpl resizeImageService;

    @Value("${default-images.survey}")
    private String defaultSurveyImage;

    @Override
    public void execute(final Long surveyId, final Survey survey) {

        final Optional<Survey> surveyThumbnailImage = this.surveyService.get(surveyId, true);
        final Optional<Survey> surveyOriginalImage = this.surveyService.get(surveyId, false);

        if (surveyThumbnailImage.isEmpty() || surveyOriginalImage.isEmpty()) {
            throw new NotFoundException("No existe la encuesta que desea actualizar");
        }

        if (survey.getImage() != null && survey.getImage().equals(surveyThumbnailImage.get().getImage())) {
            survey.setImage(surveyOriginalImage.get().getImage());
        }

        // si estan enviando imagen, debemos redimensionarla
        if (StringUtils.isNotEmpty(survey.getImage()) && !survey.getImage().equals(this.defaultSurveyImage)) {
            survey.setImageThumbnail(this.resizeImageService.resizeImage(survey.getImage()));
        }

        this.surveyService.update(surveyId, survey);
    }

}
