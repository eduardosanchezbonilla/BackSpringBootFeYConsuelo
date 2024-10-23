package com.feyconsuelo.infrastructure.converter.userpartituregroup;

import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupResponse;
import com.feyconsuelo.infrastructure.entities.userpartituregroup.UserPartitureGroupEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserPartitureGroupEntityListToUserPartitureGroupResponseListConverter {

    private final UserPartitureGroupEntityToUserPartitureGroupResponseConverter userPartitureGroupEntityToUserPartitureGroupResponseConverter;

    public List<UserPartitureGroupResponse> convert(final List<UserPartitureGroupEntity> userPartitureGroupEntityList) {
        if (CollectionUtils.isEmpty(userPartitureGroupEntityList)) {
            return List.of();
        }
        return userPartitureGroupEntityList.stream()
                .map(this.userPartitureGroupEntityToUserPartitureGroupResponseConverter::convert)
                .toList();
    }
}
