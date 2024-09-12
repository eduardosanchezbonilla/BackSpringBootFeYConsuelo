package com.feyconsuelo.infrastructure.converter.user;

import com.feyconsuelo.domain.model.user.UserRequest;
import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

@ExtendWith(MockitoExtension.class)
class UserToUserRequestEntityConverterTest {
    @InjectMocks
    private UserRequestToUserEntityConverter userRequestToUserEntityConverter;

    @Test
    void convertWithRoles() {
        // setup
        final String username = "username";
        final String password = "password";
        final List<String> roles = List.of("role");

        final UserRequest userRequest = UserRequest.builder()
                .username(username)
                .password(password)
                .roles(roles)
                .build();

        // call
        final UserEntity result = this.userRequestToUserEntityConverter.convert(userRequest);

        // assert
        Assertions.assertEquals(username, result.getUsername());
        Assertions.assertEquals(password, result.getPassword());
        Assertions.assertEquals(roles.size(), result.getRoles().size());
    }

    @Test
    void convertWithoutRoles() {
        // setup
        final String username = "username";
        final String password = "password";
        final List<String> roles = List.of();

        final UserRequest userRequest = UserRequest.builder()
                .username(username)
                .password(password)
                .roles(roles)
                .build();

        // call
        final UserEntity result = this.userRequestToUserEntityConverter.convert(userRequest);

        // assert
        Assertions.assertEquals(username, result.getUsername());
        Assertions.assertEquals(password, result.getPassword());
        Assertions.assertTrue(result.getRoles().isEmpty());
    }
}