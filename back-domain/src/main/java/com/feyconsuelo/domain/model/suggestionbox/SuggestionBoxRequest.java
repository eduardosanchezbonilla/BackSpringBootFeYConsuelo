package com.feyconsuelo.domain.model.suggestionbox;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class SuggestionBoxRequest {
    private Long id;
    private String username;
    private String suggestion;
    private Boolean readed;

}
