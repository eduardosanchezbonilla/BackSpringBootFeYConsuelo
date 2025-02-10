package com.feyconsuelo.infrastructure.converter.suggestionbox;

import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxResponse;
import com.feyconsuelo.infrastructure.entities.suggestionbox.SuggestionBoxEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class SuggestionBoxEntityListToSuggestionBoxResponseListConverter {

    private final SuggestionBoxEntityToSuggestionBoxResponseConverter suggestionBoxEntityToSuggestionBoxResponseConverter;

    public List<SuggestionBoxResponse> convert(final List<SuggestionBoxEntity> suggestionBoxEntityList) {
        if (CollectionUtils.isEmpty(suggestionBoxEntityList)) {
            return List.of();
        }
        return suggestionBoxEntityList.stream()
                .map(this.suggestionBoxEntityToSuggestionBoxResponseConverter::convert)
                .toList();
    }
}
