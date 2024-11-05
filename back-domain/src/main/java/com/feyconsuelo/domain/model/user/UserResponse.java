package com.feyconsuelo.domain.model.user;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
public class UserResponse {

    private String username;

    private String password;

    private List<String> roles;

    private LocalDateTime deletedDate;

    private Boolean passwordExpired;

    private String dni;

    private String name;

    private String surname;

    private String direction;

    private String municipality;

    private String province;

    private String email;

    private String description;

    private String image;

    private List<String> firebaseToken;

    private LocalDateTime lastAccessDate;

}
