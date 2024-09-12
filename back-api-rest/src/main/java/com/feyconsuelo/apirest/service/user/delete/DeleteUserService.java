package com.feyconsuelo.apirest.service.user.delete;

import com.feyconsuelo.domain.usecase.user.DeleteUser;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteUserService {

    private final DeleteUser deleteUser;

    public ResponseEntity<Void> deleteUser(final String username) {
        this.deleteUser.execute(username.toLowerCase());
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
