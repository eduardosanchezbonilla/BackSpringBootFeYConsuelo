package com.feyconsuelo.apirest.service.user.update;

import com.feyconsuelo.domain.model.user.ResetUserPasswordRequest;
import com.feyconsuelo.domain.usecase.user.ResetUserPassword;
import com.feyconsuelo.openapi.model.ResetUserPasswordRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class ResetUserPasswordService {

    private final ResetUserPassword resetUserPassword;

    public ResponseEntity<Void> resetUserPassword(final String username,
                                                  final ResetUserPasswordRequestDto resetUserPasswordRequestDto) {
        this.resetUserPassword.execute(
                ResetUserPasswordRequest.builder()
                        .username(username.toLowerCase())
                        .password(resetUserPasswordRequestDto.getPassword())
                        .repeatPassword(resetUserPasswordRequestDto.getRepeatPassword())
                        .build()
        );
        return ResponseEntity.status(HttpStatus.OK).build();
    }
}
