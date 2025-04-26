package com.feyconsuelo.domain.model.contactrequest;

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
public class ContactRequest {
    private Long id;
    private String name;
    private String phoneNumber;
    private String email;
    private String message;
    private Boolean readed;

}
