package com.feyconsuelo.apirest.converter.suggestionbox;

import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxGroupByUserResponse;
import com.feyconsuelo.openapi.model.SuggestionBoxGroupByUserResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class SuggestionBoxGroupByUserResponseListToSuggestionBoxGroupByUserResponseDtoListConverter {

    private final SuggestionBoxGroupByUserResponseToSuggestionBoxGroupByUserResponseDtoConverter suggestionBoxGroupByUserResponseToSuggestionBoxGroupByUserResponseDtoConverter;

    public List<SuggestionBoxGroupByUserResponseDto> convert(final List<SuggestionBoxGroupByUserResponse> suggestionBoxGroupByUserResponseList) {
        if (CollectionUtils.isEmpty(suggestionBoxGroupByUserResponseList)) {
            return List.of();
        }
        return suggestionBoxGroupByUserResponseList.stream()
                .map(this.suggestionBoxGroupByUserResponseToSuggestionBoxGroupByUserResponseDtoConverter::convert)
                .toList();
    }

}
