package com.feyconsuelo.domain.model.user;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
public class UserRequest {

    private String username;

    private List<String> roles;

    private String password;

    private Boolean passwordExpired;

    private LocalDateTime deletedDate;

    private String name;

    private String dni;

    private String direction;

    private String surname;

    private String province;

    private String municipality;

    private String description;

    private String email;

    private String image;

    private String phoneNumber;

    private String imageThumbnail;

}
