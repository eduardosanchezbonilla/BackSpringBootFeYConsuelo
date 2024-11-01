package com.feyconsuelo.domain.model.user;

import lombok.Builder;
import lombok.Value;

@Value
@Builder
public class UpdateUserFirebaseTokenRequest {

    String username;

    String firebaseToken;

}
