package com.feyconsuelo.application.usecase.userpartituregroup;

import com.feyconsuelo.application.service.userpartituregroup.UserPartitureGroupService;
import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupRequest;
import com.feyconsuelo.domain.usecase.userpartituregroup.InsertUserPartitureGroup;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertUserPartitureGroupImpl implements InsertUserPartitureGroup {

    private final UserPartitureGroupService userPartitureGroupService;

    @Override
    public void execute(final UserPartitureGroupRequest userPartitureGroupRequest) {

        this.userPartitureGroupService.insert(userPartitureGroupRequest);
    }
}
