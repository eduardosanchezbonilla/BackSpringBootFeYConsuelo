package com.feyconsuelo.apirest.service.user.update;

import com.feyconsuelo.domain.model.user.UpdateUserFirebaseTokenRequest;
import com.feyconsuelo.domain.usecase.user.UpdateUserFirebaseToken;
import com.feyconsuelo.openapi.model.UpdateUserFirebaseTokenRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateUserFirebaseTokenService {

    private final UpdateUserFirebaseToken updateUserFirebaseToken;

    public ResponseEntity<Void> updateUserFirebaseToken(final String username,
                                                        final UpdateUserFirebaseTokenRequestDto updateUserFirebaseTokenRequestDto) {
        this.updateUserFirebaseToken.execute(
                UpdateUserFirebaseTokenRequest.builder()
                        .username(username.toLowerCase())
                        .firebaseToken(updateUserFirebaseTokenRequestDto.getFirebaseToken())
                        .build()
        );
        return ResponseEntity.status(HttpStatus.OK).build();
    }
}
