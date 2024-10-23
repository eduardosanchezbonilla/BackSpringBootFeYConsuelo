package com.feyconsuelo.infrastructure.converter.userpartituregroup;

import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupRequest;
import com.feyconsuelo.infrastructure.entities.userpartituregroup.UserPartitureGroupEntity;
import com.feyconsuelo.infrastructure.entities.userpartituregroup.UserPartitureGroupPK;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserPartitureGroupRequestToUserPartitureGroupEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public UserPartitureGroupEntity convert(final UserPartitureGroupRequest userPartitureGroupRequest) {
        return UserPartitureGroupEntity.builder()
                .id(
                        UserPartitureGroupPK.builder()
                                .username(userPartitureGroupRequest.getUsername())
                                .partitureGroupId(userPartitureGroupRequest.getPartitureGroupId())
                                .build()
                )
                .deleteDate(userPartitureGroupRequest.getDeletedDate())
                .updateUser(this.tokenInfoExtractorService.getUsername())
                .build();
    }

}
