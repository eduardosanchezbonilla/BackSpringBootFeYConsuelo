package com.feyconsuelo.apirest.converter.user;

import com.feyconsuelo.domain.model.user.UserGroupByRoleResponse;
import com.feyconsuelo.domain.model.user.UserRoleEnum;
import com.feyconsuelo.openapi.model.UserGroupByRoleResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserGroupByRoleResponseToUserGroupByRoleResponseDtoConverter {

    private final UserMusicianResponseListToUserResponseDtoListConverter userMusicianResponseListToUserResponseDtoListConverter;

    private String getRoleName(final String role) {
        try {
            return UserRoleEnum.of(role).getRoleName();
        } catch (final Exception e) {
            log.error("Error getting role name", e);
            return role;
        }
    }

    public UserGroupByRoleResponseDto convert(final UserGroupByRoleResponse userGroupByRoleResponse) {
        return UserGroupByRoleResponseDto.builder()
                .role(userGroupByRoleResponse.getRole())
                .roleName(this.getRoleName(userGroupByRoleResponse.getRole()))
                .users(this.userMusicianResponseListToUserResponseDtoListConverter.convert(userGroupByRoleResponse.getUsers()))
                .build();
    }

}
