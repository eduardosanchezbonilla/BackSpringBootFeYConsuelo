package com.feyconsuelo.apirest.service.userpatituregroup.query;

import com.feyconsuelo.apirest.converter.userpartituregroup.UserPartitureGroupResponseListToUserPartitureGroupResponseDtoListConverter;
import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupResponse;
import com.feyconsuelo.domain.usecase.userpartituregroup.GetUserPartitureGroups;
import com.feyconsuelo.openapi.model.UserPartitureGroupResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetUserPartitureGroupsService {

    private final GetUserPartitureGroups getUserPartitureGroups;

    private final UserPartitureGroupResponseListToUserPartitureGroupResponseDtoListConverter userPartitureGroupResponseListToUserPartitureGroupResponseDtoListConverter;

    public ResponseEntity<List<UserPartitureGroupResponseDto>> getUserPartitureGroups(final String username) {
        final List<UserPartitureGroupResponse> userResponseList = this.getUserPartitureGroups.execute(username.toLowerCase());
        if (CollectionUtils.isEmpty(userResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.userPartitureGroupResponseListToUserPartitureGroupResponseDtoListConverter.convert(userResponseList));
    }

}
