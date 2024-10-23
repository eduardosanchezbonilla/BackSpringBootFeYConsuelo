package com.feyconsuelo.apirest.converter.userpartituregroup;

import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupResponse;
import com.feyconsuelo.openapi.model.UserPartitureGroupResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserPartitureGroupResponseListToUserPartitureGroupResponseDtoListConverter {

    private final UserPartitureGroupResponseToUserPartitureGroupResponseDtoConverter userPartitureGroupResponseToUserPartitureGroupResponseDtoConverter;

    public List<UserPartitureGroupResponseDto> convert(final List<UserPartitureGroupResponse> userPartitureGroupResponseList) {
        if (CollectionUtils.isEmpty(userPartitureGroupResponseList)) {
            return List.of();
        }
        return userPartitureGroupResponseList.stream()
                .map(this.userPartitureGroupResponseToUserPartitureGroupResponseDtoConverter::convert)
                .sorted(Comparator.comparing(UserPartitureGroupResponseDto::getName))
                .toList();
    }

}
