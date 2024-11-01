package com.feyconsuelo.apirest.converter.user;

import com.feyconsuelo.domain.model.user.UserGroupByRoleResponse;
import com.feyconsuelo.openapi.model.UserGroupByRoleResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserGroupByRoleResponseToUserGroupByRoleResponseDtoConverter {

    private final UserMusicianResponseListToUserResponseDtoListConverter musicianGroupByVoiceListResponseToMusicianGroupByVoiceListResponseDtoConverter;

    public UserGroupByRoleResponseDto convert(final UserGroupByRoleResponse userGroupByRoleResponse) {
        return UserGroupByRoleResponseDto.builder()
                .role(userGroupByRoleResponse.getRole())
                .users(this.musicianGroupByVoiceListResponseToMusicianGroupByVoiceListResponseDtoConverter.convert(userGroupByRoleResponse.getUsers()))
                .build();
    }

}
