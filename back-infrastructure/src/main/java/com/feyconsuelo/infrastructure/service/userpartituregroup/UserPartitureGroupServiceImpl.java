package com.feyconsuelo.infrastructure.service.userpartituregroup;

import com.feyconsuelo.application.service.userpartituregroup.UserPartitureGroupService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupRequest;
import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupResponse;
import com.feyconsuelo.infrastructure.converter.user.UserEntityToUserResponseConverter;
import com.feyconsuelo.infrastructure.converter.userpartituregroup.UserPartitureGroupEntityListToUserPartitureGroupResponseListConverter;
import com.feyconsuelo.infrastructure.converter.userpartituregroup.UserPartitureGroupRequestToUserPartitureGroupEntityConverter;
import com.feyconsuelo.infrastructure.entities.userpartituregroup.UserPartitureGroupEntity;
import com.feyconsuelo.infrastructure.entities.userpartituregroup.UserPartitureGroupPK;
import com.feyconsuelo.infrastructure.repository.UserPartitureGroupRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserPartitureGroupServiceImpl implements UserPartitureGroupService {

    private final UserPartitureGroupRepository userPartitureGroupRepository;
    private final UserPartitureGroupRequestToUserPartitureGroupEntityConverter userPartitureGroupRequestToUserPartitureGroupEntityConverter;
    private final UserPartitureGroupEntityListToUserPartitureGroupResponseListConverter userPartitureGroupEntityListToUserPartitureGroupResponseListConverter;
    private final UserEntityToUserResponseConverter userEntityToUserResponseConverter;

    @Override
    public void insert(final UserPartitureGroupRequest userPartitureGroupRequest) {
        this.userPartitureGroupRepository.save(
                this.userPartitureGroupRequestToUserPartitureGroupEntityConverter.convert(userPartitureGroupRequest)
        );
    }

    @Override
    public void logicalDelete(final UserPartitureGroupRequest userPartitureGroupRequest) {

        final var userPartitureGroup = this.userPartitureGroupRepository.findUserPartitureGroupActiveById(
                userPartitureGroupRequest.getUsername(),
                userPartitureGroupRequest.getPartitureGroupId()
        );

        if (userPartitureGroup.isEmpty()) {
            throw new NotFoundException("No existe el registro que intenta eliminar");
        }

        userPartitureGroup.get().setDeleteDate(LocalDateTime.now());
        this.userPartitureGroupRepository.save(userPartitureGroup.get());
    }

    @Override
    public void delete(final UserPartitureGroupRequest userPartitureGroupRequest) {
        this.userPartitureGroupRepository.deleteById(
                UserPartitureGroupPK.builder()
                        .username(userPartitureGroupRequest.getUsername())
                        .partitureGroupId(userPartitureGroupRequest.getPartitureGroupId())
                        .build()
        );
    }

    @Override
    public List<UserPartitureGroupResponse> getAllUserPartitureGroups(final String username) {
        final List<UserPartitureGroupEntity> userPartitureGroups = this.userPartitureGroupRepository.findAllActivesByUser(username);
        return this.userPartitureGroupEntityListToUserPartitureGroupResponseListConverter.convert(userPartitureGroups);
    }

    @Override
    public List<UserPartitureGroupResponse> getUserWithPartitureGroup(final Long partitureGroupId) {
        final List<UserPartitureGroupEntity> userPartitureGroups = this.userPartitureGroupRepository.findUserWithPartitureGroup(partitureGroupId);
        return this.userPartitureGroupEntityListToUserPartitureGroupResponseListConverter.convert(userPartitureGroups);
    }

}
