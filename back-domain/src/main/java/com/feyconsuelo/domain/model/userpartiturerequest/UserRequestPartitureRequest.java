package com.feyconsuelo.domain.model.userpartiturerequest;

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
public class UserRequestPartitureRequest {
    private Long id;
    private String username;
    private String description;
    private Boolean readed;
    private String markReadUnreadNotificationMessage;

}
