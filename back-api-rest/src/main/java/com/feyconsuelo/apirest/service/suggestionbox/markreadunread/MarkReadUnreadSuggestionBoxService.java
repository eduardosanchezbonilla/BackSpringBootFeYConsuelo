package com.feyconsuelo.apirest.service.suggestionbox.markreadunread;

import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxRequest;
import com.feyconsuelo.domain.usecase.suggestionbox.MarkReadUnreadSuggestionBox;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class MarkReadUnreadSuggestionBoxService {

    private final MarkReadUnreadSuggestionBox markReadUnreadSuggestionBox;

    public ResponseEntity<Void> markReadUnread(final SuggestionBoxRequest suggestionBoxRequest) {
        this.markReadUnreadSuggestionBox.execute(suggestionBoxRequest);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
