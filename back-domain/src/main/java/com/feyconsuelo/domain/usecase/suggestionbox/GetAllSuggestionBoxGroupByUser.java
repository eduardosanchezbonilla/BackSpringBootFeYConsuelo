package com.feyconsuelo.domain.usecase.suggestionbox;

import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxGroupByUserResponse;

import java.util.List;

public interface GetAllSuggestionBoxGroupByUser {

    List<SuggestionBoxGroupByUserResponse> execute(Boolean all);

}
