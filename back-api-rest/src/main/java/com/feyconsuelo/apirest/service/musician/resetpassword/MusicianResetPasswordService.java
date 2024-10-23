package com.feyconsuelo.apirest.service.musician.resetpassword;

import com.feyconsuelo.domain.usecase.musician.ResetPasswordMusician;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class MusicianResetPasswordService {

    private final ResetPasswordMusician resetPasswordMusician;

    public ResponseEntity<Void> resetPassword(final String dni) {
        this.resetPasswordMusician.execute(dni);

        return ResponseEntity.status(HttpStatus.OK).build();
    }
}
