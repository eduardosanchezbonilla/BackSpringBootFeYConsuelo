package com.feyconsuelo.apirest.service.user.update;

import com.feyconsuelo.domain.model.user.UpdateUserPasswordRequest;
import com.feyconsuelo.domain.usecase.user.UpdateUserPassword;
import com.feyconsuelo.openapi.model.UpdateUserPasswordRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateUserPasswordService {

    private final UpdateUserPassword updateUserPassword;

    public ResponseEntity<Void> updateUserPassword(final String username,
                                                   final UpdateUserPasswordRequestDto updateUserPasswordDto) {
        this.updateUserPassword.execute(
                UpdateUserPasswordRequest.builder()
                        .username(username.toLowerCase())
                        .currentPassword(updateUserPasswordDto.getCurrentPassword())
                        .newPassword(updateUserPasswordDto.getNewPassword())
                        .repeatNewPassword(updateUserPasswordDto.getRepeatNewPassword())
                        .build()
        );
        return ResponseEntity.status(HttpStatus.OK).build();
    }
}
