package com.feyconsuelo.application.usecase.userpartituregroup;

import com.feyconsuelo.application.service.userpartituregroup.UserPartitureGroupService;
import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupRequest;
import com.feyconsuelo.domain.usecase.userpartituregroup.DeleteUserPartitureGroup;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteUserPartitureGroupImpl implements DeleteUserPartitureGroup {

    private final UserPartitureGroupService userPartitureGroupService;

    @Override
    public void execute(final UserPartitureGroupRequest userPartitureGroupRequest) {
        this.userPartitureGroupService.logicalDelete(userPartitureGroupRequest);
    }
}
