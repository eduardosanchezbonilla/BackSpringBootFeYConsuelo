package com.feyconsuelo.application.service.suggestionbox;

import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxRequest;
import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxResponse;

import java.util.List;

public interface SuggestionBoxService {

    void insert(SuggestionBoxRequest suggestionBoxRequest);

    void logicalDelete(SuggestionBoxRequest suggestionBoxRequest);

    List<SuggestionBoxResponse> getAllSuggestionBox();

    List<SuggestionBoxResponse> getAllSuggestionBoxByUser(String username);

    void markReadUnread(SuggestionBoxRequest suggestionBoxRequest);

}
