package com.feyconsuelo.domain.usecase.suggestionbox;

import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxRequest;

public interface MarkReadUnreadSuggestionBox {

    void execute(final SuggestionBoxRequest suggestionBoxRequest);

}
