package com.feyconsuelo.infrastructure.converter.user;

import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserEntityListToUserResponseListConverter {

    private final UserEntityToUserResponseConverter userEntityToUserResponseConverter;

    public List<UserResponse> convert(final List<UserEntity> userEntityList) {
        if (CollectionUtils.isEmpty(userEntityList)) {
            return List.of();
        }
        return userEntityList.stream()
                .map(user -> this.userEntityToUserResponseConverter.convert(user, Boolean.TRUE))
                .toList();
    }
}
