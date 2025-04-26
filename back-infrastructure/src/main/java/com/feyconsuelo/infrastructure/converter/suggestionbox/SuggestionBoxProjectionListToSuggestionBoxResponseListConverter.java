package com.feyconsuelo.infrastructure.converter.suggestionbox;

import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxResponse;
import com.feyconsuelo.infrastructure.entities.suggestionbox.SuggestionBoxProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class SuggestionBoxProjectionListToSuggestionBoxResponseListConverter {

    private final SuggestionBoxProjectionToSuggestionBoxResponseConverter suggestionBoxProjectionToSuggestionBoxResponseConverter;

    public List<SuggestionBoxResponse> convert(final List<SuggestionBoxProjection> suggestionBoxEntityList) {
        if (CollectionUtils.isEmpty(suggestionBoxEntityList)) {
            return List.of();
        }
        return suggestionBoxEntityList.stream()
                .map(this.suggestionBoxProjectionToSuggestionBoxResponseConverter::convert)
                .toList();
    }
}
