package com.feyconsuelo.apirest.converter.userpartituregroup;

import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupResponse;
import com.feyconsuelo.openapi.model.UserPartitureGroupResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserPartitureGroupResponseToUserPartitureGroupResponseDtoConverter {

    public UserPartitureGroupResponseDto convert(final UserPartitureGroupResponse userPartitureGroupResponse) {
        return UserPartitureGroupResponseDto.builder()
                .id(userPartitureGroupResponse.getPartitureGroupId())
                .name(userPartitureGroupResponse.getPartitureGroupName())
                .assigned(userPartitureGroupResponse.getAssigned())
                .build();
    }

}
