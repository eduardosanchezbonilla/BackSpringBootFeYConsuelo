package com.feyconsuelo.apirest.converter.userpratiturerequest;

import com.feyconsuelo.apirest.converter.user.UserResponseToUserResponseDtoConverter;
import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureGroupByUserResponse;
import com.feyconsuelo.openapi.model.UserRequestPartitureGroupByUserResponseDto;
import com.feyconsuelo.openapi.model.UserRequestPartitureResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserRequestPartitureGroupByUserResponseToUserRequestPartitureGroupByUserResponseDtoConverter {

    private final UserRequestPartitureResponseToUserRequestPartitureResponseDtoConverter userRequestPartitureResponseToUserRequestPartitureResponseDtoConverter;
    private final UserResponseToUserResponseDtoConverter userResponseToUserResponseDtoConverter;

    public UserRequestPartitureGroupByUserResponseDto convert(final UserRequestPartitureGroupByUserResponse userRequestPartitureGroupByUserResponse) {
        return UserRequestPartitureGroupByUserResponseDto.builder()
                .user(this.userResponseToUserResponseDtoConverter.convert(userRequestPartitureGroupByUserResponse.getUser()))
                .request(
                        CollectionUtils.isEmpty(userRequestPartitureGroupByUserResponse.getRequest())
                                ? List.of()
                                : userRequestPartitureGroupByUserResponse.getRequest().stream()
                                .map(this.userRequestPartitureResponseToUserRequestPartitureResponseDtoConverter::convert)
                                .sorted(Comparator.comparing(UserRequestPartitureResponseDto::getId))
                                .toList()
                )
                .build();
    }

}
