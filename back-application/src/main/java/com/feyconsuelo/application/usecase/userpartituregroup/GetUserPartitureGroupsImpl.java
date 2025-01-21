package com.feyconsuelo.application.usecase.userpartituregroup;

import com.feyconsuelo.application.service.partituregroup.PartitureGroupService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.application.service.userpartituregroup.UserPartitureGroupService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.partituregroup.PartitureGroupResponse;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.model.user.UserRoleEnum;
import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupResponse;
import com.feyconsuelo.domain.usecase.userpartituregroup.GetUserPartitureGroups;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetUserPartitureGroupsImpl implements GetUserPartitureGroups {

    private final PartitureGroupService partitureGroupService;
    private final UserPartitureGroupService userPartitureGroupService;
    private final UserService userService;

    @Override
    public List<UserPartitureGroupResponse> execute(final String username) {

        final Optional<UserResponse> userResponse = this.userService.get(username, Boolean.TRUE);

        if (userResponse.isEmpty()) {
            throw new NotFoundException("No existe el usuario");
        }

        // obtenemos todos los gruipos
        final List<PartitureGroupResponse> allPartitureGroups = this.partitureGroupService.getAll(List.of(), true);

        // obtenemos los grupos asignados al usuario
        final List<UserPartitureGroupResponse> userPartitureGroups = this.userPartitureGroupService.getAllUserPartitureGroups(username);

        if (CollectionUtils.isEmpty(allPartitureGroups)) {
            return List.of();
        } else {
            return allPartitureGroups.stream()
                    .map(group ->
                            UserPartitureGroupResponse.builder()
                                    .username(username)
                                    .partitureGroupId(group.getId())
                                    .partitureGroupName(group.getName())
                                    .assigned(userResponse.get().getRoles().contains(UserRoleEnum.SUPER_ADMIN.getId()) || userPartitureGroups.stream().anyMatch(userGroup -> userGroup.getPartitureGroupId().equals(group.getId())))
                                    .build()
                    )
                    .toList();
        }
    }
}
