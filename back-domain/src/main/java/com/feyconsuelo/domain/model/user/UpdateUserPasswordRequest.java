package com.feyconsuelo.domain.model.user;

import lombok.Builder;
import lombok.Value;

@Value
@Builder
public class UpdateUserPasswordRequest {

    String username;

    String currentPassword;

    String newPassword;

    String repeatNewPassword;

}
