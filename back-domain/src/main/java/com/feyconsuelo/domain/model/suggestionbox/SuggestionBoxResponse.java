package com.feyconsuelo.domain.model.suggestionbox;

import com.feyconsuelo.domain.model.user.UserResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class SuggestionBoxResponse {
    private Long id;
    private UserResponse user;
    private String suggestion;
    private Boolean readed;
    private LocalDateTime deleteDate;

}
