package com.feyconsuelo.infrastructure.converter.suggestionbox;

import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxResponse;
import com.feyconsuelo.infrastructure.converter.user.UserEntityToUserResponseConverter;
import com.feyconsuelo.infrastructure.entities.suggestionbox.SuggestionBoxEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class SuggestionBoxEntityToSuggestionBoxResponseConverter {

    private final UserEntityToUserResponseConverter userEntityToUserResponseConverter;

    public SuggestionBoxResponse convert(final SuggestionBoxEntity suggestionBoxEntity) {
        return SuggestionBoxResponse.builder()
                .id(suggestionBoxEntity.getId())
                .user(this.userEntityToUserResponseConverter.convert(suggestionBoxEntity.getUser(), Boolean.TRUE))
                .suggestion(suggestionBoxEntity.getSuggestion())
                .readed(suggestionBoxEntity.getReaded())
                .deleteDate(suggestionBoxEntity.getDeleteDateSuggestionBox())
                .build();
    }

}
