package com.feyconsuelo.apirest.converter.user;

import com.feyconsuelo.domain.model.user.UserGroupByRoleResponse;
import com.feyconsuelo.openapi.model.UserGroupByRoleResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserGroupByRoleListResponseToUserGroupByRoleListResponseDtoConverter {

    private final UserGroupByRoleResponseToUserGroupByRoleResponseDtoConverter userGroupByRoleResponseToUserGroupByRoleResponseDtoConverter;

    private Integer getOrderRole(final String role) {
        return switch (role) {
            case "SUPER_ADMIN" -> 1;
            case "ADMIN" -> 2;
            case "MUSICO" -> 3;
            default -> 4;
        };
    }

    public List<UserGroupByRoleResponseDto> convert(final List<UserGroupByRoleResponse> userGroupByRoleResponseList) {
        if (CollectionUtils.isEmpty(userGroupByRoleResponseList)) {
            return List.of();
        }
        return userGroupByRoleResponseList.stream()
                .map(this.userGroupByRoleResponseToUserGroupByRoleResponseDtoConverter::convert)
                .sorted(Comparator.comparing(userGroupByRoleResponseDto -> this.getOrderRole(userGroupByRoleResponseDto.getRole())))
                .toList();
    }

}
