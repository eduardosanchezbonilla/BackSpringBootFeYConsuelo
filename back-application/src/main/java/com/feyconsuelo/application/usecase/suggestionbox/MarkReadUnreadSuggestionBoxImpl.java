package com.feyconsuelo.application.usecase.suggestionbox;

import com.feyconsuelo.application.service.suggestionbox.SuggestionBoxService;
import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxRequest;
import com.feyconsuelo.domain.usecase.suggestionbox.MarkReadUnreadSuggestionBox;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class MarkReadUnreadSuggestionBoxImpl implements MarkReadUnreadSuggestionBox {

    private final SuggestionBoxService suggestionBoxService;

    @Override
    public void execute(final SuggestionBoxRequest suggestionBoxRequest) {
        // registramos la soliccitud
        this.suggestionBoxService.markReadUnread(suggestionBoxRequest);
    }
}
