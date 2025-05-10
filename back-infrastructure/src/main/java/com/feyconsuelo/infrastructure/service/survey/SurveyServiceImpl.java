package com.feyconsuelo.infrastructure.service.survey;

import com.feyconsuelo.application.service.survey.SurveyService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.survey.Survey;
import com.feyconsuelo.domain.model.survey.SurveyOption;
import com.feyconsuelo.domain.model.survey.SurveyVote;
import com.feyconsuelo.infrastructure.converter.survey.SurveyProjectionListToSurveyListConverter;
import com.feyconsuelo.infrastructure.converter.survey.SurveyProjectionToSurveyConverter;
import com.feyconsuelo.infrastructure.converter.survey.SurveyToSurveyEntityConverter;
import com.feyconsuelo.infrastructure.converter.survey.SurveyVoteToUserPointSurveyVoteEntityListConverter;
import com.feyconsuelo.infrastructure.entities.survey.SurveyEntity;
import com.feyconsuelo.infrastructure.entities.survey.SurveyOptionEntity;
import com.feyconsuelo.infrastructure.repository.SurveyOptionRepository;
import com.feyconsuelo.infrastructure.repository.SurveyRepository;
import com.feyconsuelo.infrastructure.repository.UserPointSurveyOptionRepository;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class SurveyServiceImpl implements SurveyService {

    private final SurveyRepository surveyRepository;
    private final SurveyOptionRepository surveyOptionRepository;
    private final SurveyToSurveyEntityConverter surveyToSurveyEntityConverter;
    private final SurveyProjectionListToSurveyListConverter surveyProjectionListToSurveyListConverter;
    private final SurveyProjectionToSurveyConverter surveyProjectionToSurveyConverter;
    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;
    private final UserPointSurveyOptionRepository userPointSurveyOptionRepository;
    private final SurveyVoteToUserPointSurveyVoteEntityListConverter surveyVoteToUserPointSurveyVoteEntityListConverter;

    @Override
    public void delete(final Long surveyId) {
        this.surveyRepository.deleteById(surveyId);
    }

    @Override
    @Transactional
    public void logicalDelete(final Long surveyId) {
        final Optional<SurveyEntity> surveyOptional = this.surveyRepository.findActiveBySurveyId(surveyId);

        if (surveyOptional.isEmpty()) {
            throw new NotFoundException("No existe la encuesta a eliminar");
        }

        // actualizamos todas las opciones
        final List<SurveyOptionEntity> options = this.surveyOptionRepository.findOptionActiveBySurveyId(surveyId);
        options.forEach(option -> {
            option.setDeleteDateSurveyOption(LocalDateTime.now());
            option.setModifiedUserSurveyOption(this.tokenInfoExtractorService.getUsername());
        });
        this.surveyOptionRepository.saveAll(options);

        // eliminamos la encuesta
        surveyOptional.get().setDeleteDateSurvey(LocalDateTime.now());
        surveyOptional.get().setModifiedUserSurvey(this.tokenInfoExtractorService.getUsername());
        this.surveyRepository.save(surveyOptional.get());
    }

    @Override
    public List<Survey> getAll() {
        return this.surveyProjectionListToSurveyListConverter.convert(this.surveyRepository.findAllActives(), Boolean.TRUE);
    }

    @Override
    public Optional<Survey> get(final Long surveyId, final Boolean isThumbnail) {
        final var surveyOptional = this.surveyRepository.findSurveyActiveById(this.tokenInfoExtractorService.getUsername(), surveyId);
        return surveyOptional.map(survey -> this.surveyProjectionToSurveyConverter.convert(survey, isThumbnail));
    }

    @Override
    @Transactional
    public void insert(final Survey survey) {
        // insertamos la encuesta y despues su opciones
        final SurveyEntity insertedSurvey = this.surveyRepository.save(this.surveyToSurveyEntityConverter.convert(survey));

        // insertamos las opciones
        this.surveyOptionRepository.saveAll(this.surveyToSurveyEntityConverter.convertOptions(insertedSurvey.getId(), survey.getOptions()));
    }

    @Override
    @Transactional
    public void update(final Long surveyId, final Survey survey) {
        final var surveyOptional = this.surveyRepository.findById(surveyId);

        if (surveyOptional.isEmpty()) {
            throw new NotFoundException("No existe la encuesta a modificar");
        }

        // eliminamos todas las opciones
        this.surveyOptionRepository.deleteAllOptionsSurvey(
                surveyId,
                survey.getOptions().stream()
                        .map(SurveyOption::getId)
                        .filter(Objects::nonNull)
                        .toList()
        );

        // modificamos la encuesta
        final SurveyEntity updateSurvey = this.surveyRepository.save(this.surveyToSurveyEntityConverter.updateEntity(surveyOptional.get(), survey));

        // insertamos todas las opciones
        this.surveyOptionRepository.saveAll(this.surveyToSurveyEntityConverter.convertOptions(updateSurvey.getId(), survey.getOptions()));
    }

    @Override
    @Transactional
    public void vote(final Long surveyId, final SurveyVote surveyVote) {
        final var surveyOptional = this.surveyRepository.findById(surveyId);

        if (surveyOptional.isEmpty()) {
            throw new NotFoundException("No existe la encuesta a modificar");
        }

        if (CollectionUtils.isEmpty(surveyVote.getOptions())) {
            // delete todas las opciones del username
            this.userPointSurveyOptionRepository.deleteAllOptionsSurveyByUsernameAndSurveyId(surveyVote.getUsername(), surveyId);
        } else {
            this.userPointSurveyOptionRepository.saveAll(this.surveyVoteToUserPointSurveyVoteEntityListConverter.convert(surveyVote));
        }
    }

}
