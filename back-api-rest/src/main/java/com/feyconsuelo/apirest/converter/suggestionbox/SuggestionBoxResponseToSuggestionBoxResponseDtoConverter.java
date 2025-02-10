package com.feyconsuelo.apirest.converter.suggestionbox;

import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxResponse;
import com.feyconsuelo.openapi.model.SuggestionBoxResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class SuggestionBoxResponseToSuggestionBoxResponseDtoConverter {

    public SuggestionBoxResponseDto convert(final SuggestionBoxResponse suggestionBoxResponse) {
        return SuggestionBoxResponseDto.builder()
                .id(suggestionBoxResponse.getId())
                .username(suggestionBoxResponse.getUser().getUsername())
                .suggestion(suggestionBoxResponse.getSuggestion())
                .readed(suggestionBoxResponse.getReaded())
                .build();
    }

}
