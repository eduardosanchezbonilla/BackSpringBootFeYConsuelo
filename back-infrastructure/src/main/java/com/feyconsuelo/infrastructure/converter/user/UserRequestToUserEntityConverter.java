package com.feyconsuelo.infrastructure.converter.user;

import com.feyconsuelo.domain.model.user.UserRequest;
import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import com.feyconsuelo.infrastructure.entities.user.UserRoleEntity;
import com.feyconsuelo.infrastructure.entities.user.UserRolePK;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserRequestToUserEntityConverter {

    public UserEntity convert(final UserRequest userRequest) {
        return UserEntity.builder()
                .username(userRequest.getUsername())
                .password(userRequest.getPassword())
                .roles(CollectionUtils.isEmpty(userRequest.getRoles()) ?
                        List.of() :
                        userRequest.getRoles().stream()
                                .map(role -> UserRoleEntity.builder()
                                        .id(UserRolePK.builder()
                                                .username(userRequest.getUsername())
                                                .role(role)
                                                .build()
                                        )
                                        .deleteDate(userRequest.getDeletedDate())
                                        .build()
                                )
                                .toList()
                )
                .deleteDate(userRequest.getDeletedDate())
                .build();
    }

}
