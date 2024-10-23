package com.feyconsuelo.application.usecase.partituregroup;

import com.feyconsuelo.application.service.partituregroup.PartitureGroupService;
import com.feyconsuelo.application.service.user.TokenInfoExtractorService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.application.service.userpartituregroup.UserPartitureGroupService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.partituregroup.PartitureGroupResponse;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.model.user.UserRoleEnum;
import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupResponse;
import com.feyconsuelo.domain.usecase.partituregroup.GetAllPartitureGroups;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetAllPartitureGroupsImpl implements GetAllPartitureGroups {

    private final PartitureGroupService partitureGroupService;
    private final TokenInfoExtractorService tokenInfoExtractorService;
    private final UserService userService;
    private final UserPartitureGroupService userPartitureGroupService;

    @Override
    public List<PartitureGroupResponse> execute() {

        final List<Long> partitureGroupsIdsAccess;
        final boolean allPartitureGroups;

        final String username = this.tokenInfoExtractorService.getUsername();
        final Optional<UserResponse> userResponse = this.userService.get(username);

        if (userResponse.isEmpty()) {
            throw new NotFoundException("No existe el usuario");
        }

        if (userResponse.get().getRoles().contains(UserRoleEnum.SUPER_ADMIN.getId())) {
            allPartitureGroups = true;
            partitureGroupsIdsAccess = List.of();
        } else {
            final List<UserPartitureGroupResponse> userPartitureGroups = this.userPartitureGroupService.getAllUserPartitureGroups(username);
            allPartitureGroups = false;
            if (CollectionUtils.isEmpty(userPartitureGroups)) {
                partitureGroupsIdsAccess = List.of();
            } else {
                partitureGroupsIdsAccess = userPartitureGroups.stream().map(UserPartitureGroupResponse::getPartitureGroupId).toList();
            }
        }

        return this.partitureGroupService.getAll(partitureGroupsIdsAccess, allPartitureGroups);
    }
}
