package com.feyconsuelo.apirest.service.suggestionbox.insert;

import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxRequest;
import com.feyconsuelo.domain.usecase.suggestionbox.InsertSuggestionBox;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertSuggestionBoxService {

    private final InsertSuggestionBox insertSuggestionBox;

    public ResponseEntity<Void> insertSuggestionBox(final SuggestionBoxRequest suggestionBoxRequest) {
        this.insertSuggestionBox.execute(suggestionBoxRequest);
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

}
