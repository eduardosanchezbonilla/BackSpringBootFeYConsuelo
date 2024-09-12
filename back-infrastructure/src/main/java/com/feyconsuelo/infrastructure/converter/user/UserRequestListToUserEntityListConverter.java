package com.feyconsuelo.infrastructure.converter.user;

import com.feyconsuelo.domain.model.user.UserRequest;
import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserRequestListToUserEntityListConverter {

    private final UserRequestToUserEntityConverter userRequestToUserEntityConverter;

    public List<UserEntity> convert(final List<UserRequest> userRequestList) {
        if (CollectionUtils.isEmpty(userRequestList)) {
            return List.of();
        }
        return userRequestList.stream()
                .map(this.userRequestToUserEntityConverter::convert)
                .toList();
    }
}
