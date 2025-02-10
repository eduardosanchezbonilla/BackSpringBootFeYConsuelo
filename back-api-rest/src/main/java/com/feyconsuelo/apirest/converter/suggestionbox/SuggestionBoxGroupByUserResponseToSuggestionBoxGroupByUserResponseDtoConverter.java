package com.feyconsuelo.apirest.converter.suggestionbox;

import com.feyconsuelo.apirest.converter.user.UserResponseToUserResponseDtoConverter;
import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxGroupByUserResponse;
import com.feyconsuelo.openapi.model.SuggestionBoxGroupByUserResponseDto;
import com.feyconsuelo.openapi.model.SuggestionBoxResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class SuggestionBoxGroupByUserResponseToSuggestionBoxGroupByUserResponseDtoConverter {

    private final SuggestionBoxResponseToSuggestionBoxResponseDtoConverter suggestionBoxResponseToSuggestionBoxResponseDtoConverter;
    private final UserResponseToUserResponseDtoConverter userResponseToUserResponseDtoConverter;

    public SuggestionBoxGroupByUserResponseDto convert(final SuggestionBoxGroupByUserResponse suggestionBoxGroupByUserResponse) {
        return SuggestionBoxGroupByUserResponseDto.builder()
                .user(this.userResponseToUserResponseDtoConverter.convert(suggestionBoxGroupByUserResponse.getUser()))
                .suggestions(
                        CollectionUtils.isEmpty(suggestionBoxGroupByUserResponse.getSuggestions())
                                ? List.of()
                                : suggestionBoxGroupByUserResponse.getSuggestions().stream()
                                .map(this.suggestionBoxResponseToSuggestionBoxResponseDtoConverter::convert)
                                .sorted(Comparator.comparing(SuggestionBoxResponseDto::getId))
                                .toList()
                )
                .build();
    }

}
