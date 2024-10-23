package com.feyconsuelo.apirest.service.userpatituregroup;

import com.feyconsuelo.apirest.service.userpatituregroup.delete.DeleteUserPartitureGroupService;
import com.feyconsuelo.apirest.service.userpatituregroup.insert.InsertUserPartitureGroupService;
import com.feyconsuelo.apirest.service.userpatituregroup.query.GetUserPartitureGroupsService;
import com.feyconsuelo.openapi.api.UserPartitureGroupControllerApiDelegate;
import com.feyconsuelo.openapi.model.UserPartitureGroupResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserPartitureGroupApiService implements UserPartitureGroupControllerApiDelegate {

    private final DeleteUserPartitureGroupService deleteUserPartitureGroupService;
    private final InsertUserPartitureGroupService insertUserPartitureGroupService;
    private final GetUserPartitureGroupsService getUserPartitureGroupsService;

    @Override
    public ResponseEntity<Void> insertUserPartitureGroup(final String username,
                                                         final Long partitureGroupId) {
        return this.insertUserPartitureGroupService.insertUserPartitureGroup(username.toLowerCase(), partitureGroupId);
    }

    @Override
    public ResponseEntity<Void> deleteUserPartitureGroup(final String username,
                                                         final Long partitureGroupId) {
        return this.deleteUserPartitureGroupService.deleteUserPartitureGroup(username.toLowerCase(), partitureGroupId);
    }

    @Override
    public ResponseEntity<List<UserPartitureGroupResponseDto>> getUserPartitureGroup(final String username) {
        return this.getUserPartitureGroupsService.getUserPartitureGroups(username.toLowerCase());
    }

}
