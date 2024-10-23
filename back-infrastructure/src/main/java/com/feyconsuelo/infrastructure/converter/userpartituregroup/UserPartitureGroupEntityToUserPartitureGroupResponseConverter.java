package com.feyconsuelo.infrastructure.converter.userpartituregroup;

import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupResponse;
import com.feyconsuelo.infrastructure.entities.userpartituregroup.UserPartitureGroupEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserPartitureGroupEntityToUserPartitureGroupResponseConverter {

    public UserPartitureGroupResponse convert(final UserPartitureGroupEntity userPartitureGroupEntity) {
        return UserPartitureGroupResponse.builder()
                .username(userPartitureGroupEntity.getId().getUsername())
                .partitureGroupId(userPartitureGroupEntity.getId().getPartitureGroupId())
                .deleteDate(userPartitureGroupEntity.getDeleteDate())
                .build();
    }

}
