package com.feyconsuelo.infrastructure.service.suggestionbox;

import com.feyconsuelo.application.service.suggestionbox.SuggestionBoxService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxRequest;
import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxResponse;
import com.feyconsuelo.infrastructure.converter.suggestionbox.SuggestionBoxEntityListToSuggestionBoxResponseListConverter;
import com.feyconsuelo.infrastructure.converter.suggestionbox.SuggestionBoxProjectionListToSuggestionBoxResponseListConverter;
import com.feyconsuelo.infrastructure.converter.suggestionbox.SuggestionBoxRequestToSuggestionBoxEntityConverter;
import com.feyconsuelo.infrastructure.entities.suggestionbox.SuggestionBoxEntity;
import com.feyconsuelo.infrastructure.entities.suggestionbox.SuggestionBoxProjection;
import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import com.feyconsuelo.infrastructure.repository.SuggestionBoxRepository;
import com.feyconsuelo.infrastructure.repository.UserRepository;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class SuggestionBoxServiceImpl implements SuggestionBoxService {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;
    private final UserRepository userRepository;
    private final SuggestionBoxRepository suggestionBoxRepository;
    private final SuggestionBoxRequestToSuggestionBoxEntityConverter suggestionBoxRequestToSuggestionBoxEntityConverter;
    private final SuggestionBoxEntityListToSuggestionBoxResponseListConverter suggestionBoxEntityListToSuggestionBoxResponseListConverter;
    private final SuggestionBoxProjectionListToSuggestionBoxResponseListConverter suggestionBoxProjectionListToSuggestionBoxResponseListConverter;

    private UserEntity getUserEntity() {
        return this.userRepository.findUserActiveByUserName(this.tokenInfoExtractorService.getUsername())
                .orElseThrow(() -> new NotFoundException("No existe el usuario que intenta registrar la solicitud"));
    }

    @Override
    public void insert(final SuggestionBoxRequest suggestionBoxRequest) {
        this.suggestionBoxRepository.save(
                this.suggestionBoxRequestToSuggestionBoxEntityConverter.convert(suggestionBoxRequest, this.getUserEntity())
        );
    }

    @Override
    public void logicalDelete(final SuggestionBoxRequest suggestionBoxRequest) {

        final var suggestionBoxEntity = this.suggestionBoxRepository.findActiveById(suggestionBoxRequest.getId());

        if (suggestionBoxEntity.isEmpty()) {
            throw new NotFoundException("No existe el registro que intenta eliminar");
        }

        suggestionBoxEntity.get().setDeleteDateSuggestionBox(LocalDateTime.now());
        this.suggestionBoxRepository.save(suggestionBoxEntity.get());
    }

    @Override
    public List<SuggestionBoxResponse> getAllSuggestionBox() {
        final List<SuggestionBoxProjection> suggestionBoxProjectionList = this.suggestionBoxRepository.findAllActivesProjection();
        return this.suggestionBoxProjectionListToSuggestionBoxResponseListConverter.convert(suggestionBoxProjectionList);
    }

    @Override
    public List<SuggestionBoxResponse> getAllSuggestionBoxByUser(final String username) {
        final List<SuggestionBoxEntity> suggestionBoxEntityList = this.suggestionBoxRepository.findAllActivesByUser(username);
        return this.suggestionBoxEntityListToSuggestionBoxResponseListConverter.convert(suggestionBoxEntityList);
    }

    @Override
    public void markReadUnread(final SuggestionBoxRequest suggestionBoxRequest) {
        final var suggestionBoxEntity = this.suggestionBoxRepository.findActiveById(suggestionBoxRequest.getId());

        if (suggestionBoxEntity.isEmpty()) {
            throw new NotFoundException("No existe el registro que intenta actualizar");
        }

        this.suggestionBoxRepository.save(
                this.suggestionBoxRequestToSuggestionBoxEntityConverter.markReadUnreadEntity(
                        suggestionBoxEntity.get(),
                        suggestionBoxRequest
                )
        );
    }

}
