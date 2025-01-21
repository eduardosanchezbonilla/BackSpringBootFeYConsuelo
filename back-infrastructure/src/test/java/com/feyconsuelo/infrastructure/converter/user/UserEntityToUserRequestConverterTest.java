package com.feyconsuelo.infrastructure.converter.user;

import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import com.feyconsuelo.infrastructure.entities.user.UserRoleEntity;
import com.feyconsuelo.infrastructure.entities.user.UserRolePK;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

@ExtendWith(MockitoExtension.class)
class UserEntityToUserRequestConverterTest {
    @InjectMocks
    private UserEntityToUserResponseConverter userEntityToUserResponseConverter;

    @Test
    void convertWithRoles() {
        // setup
        final String username = "username";
        final String password = "password";
        final List<UserRoleEntity> roles = List.of(
                UserRoleEntity.builder()
                        .id(
                                UserRolePK.builder()
                                        .username(username)
                                        .role("role")
                                        .build()
                        )
                        .build()
        );

        final UserEntity userEntity = UserEntity.builder()
                .username(username)
                .password(password)
                .roles(roles)
                .build();

        // call
        final UserResponse result = this.userEntityToUserResponseConverter.convert(userEntity, Boolean.TRUE);

        // userRequestDto
        Assertions.assertEquals(username, result.getUsername());
        Assertions.assertEquals(password, result.getPassword());
        Assertions.assertEquals(roles.size(), result.getRoles().size());
        Assertions.assertEquals(roles.get(0).getId().getRole(), result.getRoles().get(0));
    }

    @Test
    void convertWithoutRoles() {
        // setup
        final String username = "username";
        final String password = "password";
        final List<UserRoleEntity> roles = List.of();

        final UserEntity userEntity = UserEntity.builder()
                .username(username)
                .password(password)
                .roles(roles)
                .build();

        // call
        final UserResponse result = this.userEntityToUserResponseConverter.convert(userEntity, Boolean.TRUE);

        // assert
        Assertions.assertEquals(username, result.getUsername());
        Assertions.assertEquals(password, result.getPassword());
        Assertions.assertTrue(result.getRoles().isEmpty());
    }
}