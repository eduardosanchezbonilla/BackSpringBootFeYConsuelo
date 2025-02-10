package com.feyconsuelo.infrastructure.converter.suggestionbox;

import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxRequest;
import com.feyconsuelo.infrastructure.entities.suggestionbox.SuggestionBoxEntity;
import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class SuggestionBoxRequestToSuggestionBoxEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public SuggestionBoxEntity convert(final SuggestionBoxRequest suggestionBoxRequest,
                                       final UserEntity userEntity
    ) {
        return SuggestionBoxEntity.builder()
                .user(userEntity)
                .suggestion(suggestionBoxRequest.getSuggestion())
                .readed(suggestionBoxRequest.getReaded())
                .updateUserSuggestionBox(userEntity.getUsername())
                .build();
    }

    public SuggestionBoxEntity markReadUnreadEntity(final SuggestionBoxEntity suggestionBoxEntity,
                                                    final SuggestionBoxRequest suggestionBoxRequest
    ) {
        suggestionBoxEntity.setReaded(suggestionBoxRequest.getReaded());
        suggestionBoxEntity.setUpdateUserSuggestionBox(this.tokenInfoExtractorService.getUsername());

        return suggestionBoxEntity;
    }

}
