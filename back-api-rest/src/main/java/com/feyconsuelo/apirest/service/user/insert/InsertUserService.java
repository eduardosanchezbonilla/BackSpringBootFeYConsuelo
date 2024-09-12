package com.feyconsuelo.apirest.service.user.insert;

import com.feyconsuelo.apirest.converter.user.UserRequestDtoToUserRequestConverter;
import com.feyconsuelo.apirest.validate.user.ValidateUserService;
import com.feyconsuelo.domain.usecase.user.InsertUser;
import com.feyconsuelo.openapi.model.UserRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertUserService {

    private final InsertUser insertUser;

    private final UserRequestDtoToUserRequestConverter userRequestDtoToUserRequestConverter;

    private final ValidateUserService validateUserService;

    public ResponseEntity<Void> postUser(final UserRequestDto userRequestDto) {
        validateUserService.validate(userRequestDto);
        this.insertUser.execute(this.userRequestDtoToUserRequestConverter.convert(userRequestDto));
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }
}
