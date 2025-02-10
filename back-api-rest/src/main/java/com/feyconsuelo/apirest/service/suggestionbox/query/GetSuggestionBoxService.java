package com.feyconsuelo.apirest.service.suggestionbox.query;

import com.feyconsuelo.apirest.converter.suggestionbox.SuggestionBoxGroupByUserResponseListToSuggestionBoxGroupByUserResponseDtoListConverter;
import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxGroupByUserResponse;
import com.feyconsuelo.domain.usecase.suggestionbox.GetAllSuggestionBoxGroupByUser;
import com.feyconsuelo.openapi.model.SuggestionBoxGroupByUserResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetSuggestionBoxService {

    private final GetAllSuggestionBoxGroupByUser getAllSuggestionBoxGroupByUser;

    private final SuggestionBoxGroupByUserResponseListToSuggestionBoxGroupByUserResponseDtoListConverter suggestionBoxGroupByUserResponseListToSuggestionBoxGroupByUserResponseDtoListConverter;

    public ResponseEntity<List<SuggestionBoxGroupByUserResponseDto>> getAllSuggestionBoxGroupByUser(final Boolean all) {
        final List<SuggestionBoxGroupByUserResponse> suggestionBoxGroupByUserResponseList = this.getAllSuggestionBoxGroupByUser.execute(all);
        if (CollectionUtils.isEmpty(suggestionBoxGroupByUserResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.suggestionBoxGroupByUserResponseListToSuggestionBoxGroupByUserResponseDtoListConverter.convert(suggestionBoxGroupByUserResponseList));
    }

}
