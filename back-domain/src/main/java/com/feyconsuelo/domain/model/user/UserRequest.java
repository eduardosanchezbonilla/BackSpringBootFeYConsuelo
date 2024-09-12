package com.feyconsuelo.domain.model.user;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
public class UserRequest {

    private String username;

    private String password;

    private List<String> roles;

    private LocalDateTime deletedDate;

}
