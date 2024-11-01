package com.feyconsuelo.application.usecase.user;

import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.application.service.utils.StringService;
import com.feyconsuelo.domain.model.user.UserGroupByRoleRequest;
import com.feyconsuelo.domain.model.user.UserGroupByRoleResponse;
import com.feyconsuelo.domain.model.user.UserMusicianResponse;
import com.feyconsuelo.domain.model.user.UserRoleEnum;
import com.feyconsuelo.domain.usecase.user.GetUsersGroupByRole;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetUsersGroupByRoleImpl implements GetUsersGroupByRole {

    private final StringService stringService;
    private final UserService userService;

    // en la request solo vendran un valor, pero filtraremos tanto nombre como username
    private Boolean filterUser(final UserMusicianResponse user, final UserGroupByRoleRequest userGroupByRoleRequest) {
        if (StringUtils.isEmpty(userGroupByRoleRequest.getFilter())) {
            return Boolean.TRUE;
        } else {
            final String username = user.getUserResponse().getUsername().toUpperCase();
            final String name = (this.stringService.nvl(user.getUserResponse().getName()) + " " + this.stringService.nvl(user.getUserResponse().getSurname())).toUpperCase();
            final String userMusicianName = user.getMusicianResponse() == null ? "" : (this.stringService.nvl(user.getMusicianResponse().getName()) + " " + this.stringService.nvl(user.getMusicianResponse().getSurname())).toUpperCase();

            if (username.contains(userGroupByRoleRequest.getFilter().toUpperCase())) {
                return Boolean.TRUE;
            } else if (name.contains(userGroupByRoleRequest.getFilter().toUpperCase())) {
                return Boolean.TRUE;
            } else if (userMusicianName.contains(userGroupByRoleRequest.getFilter().toUpperCase())) {
                return Boolean.TRUE;
            } else {
                return Boolean.FALSE;
            }
        }
    }

    private List<UserMusicianResponse> filterUsers(final List<UserMusicianResponse> users,
                                                   final UserGroupByRoleRequest userGroupByRoleRequest) {
        return Boolean.TRUE.equals(CollectionUtils.isEmpty(users)) ?
                users :
                users.stream()
                        .filter(user -> this.filterUser(user, userGroupByRoleRequest))
                        .toList();
    }

    private String getUserRole(final UserMusicianResponse user) {
        if (user.getUserResponse().getRoles().contains(UserRoleEnum.SUPER_ADMIN.getId())) {
            return UserRoleEnum.SUPER_ADMIN.getId();
        } else if (user.getUserResponse().getRoles().contains(UserRoleEnum.ADMIN.getId())) {
            return UserRoleEnum.ADMIN.getId();
        } else if (user.getUserResponse().getRoles().contains(UserRoleEnum.MUSICO.getId())) {
            return UserRoleEnum.MUSICO.getId();
        } else {
            return UserRoleEnum.INVITADO.getId();
        }
    }

    private List<String> getDistinctRoles(final List<UserMusicianResponse> users) {
        return users.stream()
                .map(this::getUserRole)
                .distinct()
                .toList();
    }

    @Override
    public List<UserGroupByRoleResponse> execute(final UserGroupByRoleRequest userGroupByRoleRequest) {

        // obtenemos los datos
        final List<UserMusicianResponse> users = this.userService.getAllWithMusicianData();

        final List<UserMusicianResponse> filterUsers = this.filterUsers(users, userGroupByRoleRequest);

        // obtenemos los roles
        final List<String> roles = this.getDistinctRoles(users);

        // recorremos todos los roles y en cada una de ellos metemos los usuarios que coincidan en role
        return roles.stream()
                .map(
                        role -> UserGroupByRoleResponse.builder()
                                .role(role)
                                .users(
                                        filterUsers.stream()
                                                .filter(user -> role.equals(this.getUserRole(user)))
                                                .toList()
                                )
                                .build()
                )
                .toList();
    }
}
