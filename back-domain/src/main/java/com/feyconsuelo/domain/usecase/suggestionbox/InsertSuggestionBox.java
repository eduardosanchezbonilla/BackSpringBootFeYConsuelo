package com.feyconsuelo.domain.usecase.suggestionbox;

import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxRequest;

public interface InsertSuggestionBox {

    void execute(final SuggestionBoxRequest suggestionBoxRequest);

}
