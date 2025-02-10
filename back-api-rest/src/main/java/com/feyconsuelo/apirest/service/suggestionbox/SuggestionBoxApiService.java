package com.feyconsuelo.apirest.service.suggestionbox;

import com.feyconsuelo.apirest.service.suggestionbox.insert.InsertSuggestionBoxService;
import com.feyconsuelo.apirest.service.suggestionbox.markreadunread.MarkReadUnreadSuggestionBoxService;
import com.feyconsuelo.apirest.service.suggestionbox.query.GetSuggestionBoxService;
import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxRequest;
import com.feyconsuelo.openapi.api.SuggestionBoxControllerApiDelegate;
import com.feyconsuelo.openapi.model.SuggestionBoxGroupByUserResponseDto;
import com.feyconsuelo.openapi.model.SuggestionBoxRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class SuggestionBoxApiService implements SuggestionBoxControllerApiDelegate {

    private final InsertSuggestionBoxService insertSuggestionBoxService;
    private final GetSuggestionBoxService getSuggestionBoxService;
    private final MarkReadUnreadSuggestionBoxService markReadUnreadSuggestionBoxService;

    @Override
    public ResponseEntity<Void> insertSuggestionBoxPetition(final SuggestionBoxRequestDto suggestionBoxRequestDto) {
        return this.insertSuggestionBoxService.insertSuggestionBox(
                SuggestionBoxRequest.builder()
                        .suggestion(suggestionBoxRequestDto.getSuggestion())
                        .readed(suggestionBoxRequestDto.getReaded())
                        .build()
        );
    }

    @Override
    public ResponseEntity<List<SuggestionBoxGroupByUserResponseDto>> getAllSuggestionBoxGroupByUser(final Boolean all) {
        return this.getSuggestionBoxService.getAllSuggestionBoxGroupByUser(all);
    }

    @Override
    public ResponseEntity<Void> markReadUnreadSuggestionBox(final SuggestionBoxRequestDto suggestionBoxRequestDto) {
        return this.markReadUnreadSuggestionBoxService.markReadUnread(
                SuggestionBoxRequest.builder()
                        .id(suggestionBoxRequestDto.getId())
                        .suggestion(suggestionBoxRequestDto.getSuggestion())
                        .readed(suggestionBoxRequestDto.getReaded())
                        .build()
        );
    }

}
