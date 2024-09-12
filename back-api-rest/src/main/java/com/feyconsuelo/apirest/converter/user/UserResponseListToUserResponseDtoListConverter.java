package com.feyconsuelo.apirest.converter.user;

import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.openapi.model.UserResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserResponseListToUserResponseDtoListConverter {

    private final UserResponseToUserResponseDtoConverter userResponseToUserResponseDtoConverter;

    public List<UserResponseDto> convert(final List<UserResponse> userResponseList) {
        if (CollectionUtils.isEmpty(userResponseList)) {
            return List.of();
        }
        return userResponseList.stream().map(this.userResponseToUserResponseDtoConverter::convert).toList();
    }

}
