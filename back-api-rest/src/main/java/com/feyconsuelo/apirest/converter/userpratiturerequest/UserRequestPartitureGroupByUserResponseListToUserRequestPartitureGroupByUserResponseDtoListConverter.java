package com.feyconsuelo.apirest.converter.userpratiturerequest;

import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureGroupByUserResponse;
import com.feyconsuelo.openapi.model.UserRequestPartitureGroupByUserResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserRequestPartitureGroupByUserResponseListToUserRequestPartitureGroupByUserResponseDtoListConverter {

    private final UserRequestPartitureGroupByUserResponseToUserRequestPartitureGroupByUserResponseDtoConverter userRequestPartitureGroupByUserResponseToUserRequestPartitureGroupByUserResponseDtoConverter;

    public List<UserRequestPartitureGroupByUserResponseDto> convert(final List<UserRequestPartitureGroupByUserResponse> userRequestPartitureGroupByUserResponseList) {
        if (CollectionUtils.isEmpty(userRequestPartitureGroupByUserResponseList)) {
            return List.of();
        }
        return userRequestPartitureGroupByUserResponseList.stream()
                .map(this.userRequestPartitureGroupByUserResponseToUserRequestPartitureGroupByUserResponseDtoConverter::convert)
                .toList();
    }

}
