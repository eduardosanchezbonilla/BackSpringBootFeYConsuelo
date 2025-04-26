package com.feyconsuelo.domain.model.contactrequest;

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
public class ContactResponse {
    private Long id;
    private String name;
    private String phoneNumber;
    private String email;
    private String message;
    private Boolean readed;
    private LocalDateTime createdDate;

}
