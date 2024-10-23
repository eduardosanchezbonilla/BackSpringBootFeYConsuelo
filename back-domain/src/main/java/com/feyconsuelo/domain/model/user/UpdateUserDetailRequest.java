package com.feyconsuelo.domain.model.user;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class UpdateUserDetailRequest {

    private String username;

    private String dni;

    private String name;

    private String surname;

    private String direction;

    private String municipality;

    private String province;

    private String email;

    private String description;

    private String image;

}
