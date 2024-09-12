package com.feyconsuelo.apirest.converter.user;

import com.feyconsuelo.domain.model.user.UserRequest;
import com.feyconsuelo.openapi.model.UserRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserRequestDtoToUserRequestConverter {
    public UserRequest convert(final UserRequestDto userRequestDto) {
        return UserRequest.builder()
                .username(userRequestDto.getUsername().toLowerCase())
                .password(userRequestDto.getPassword())
                .roles(userRequestDto.getRoles())
                .build();
    }

}
