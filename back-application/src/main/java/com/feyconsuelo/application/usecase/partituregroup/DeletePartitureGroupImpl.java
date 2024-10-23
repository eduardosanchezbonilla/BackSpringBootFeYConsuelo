package com.feyconsuelo.application.usecase.partituregroup;

import com.feyconsuelo.application.service.partituregroup.PartitureGroupService;
import com.feyconsuelo.application.service.userpartituregroup.UserPartitureGroupService;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupResponse;
import com.feyconsuelo.domain.usecase.partituregroup.DeletePartitureGroup;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Component
@RequiredArgsConstructor
public class DeletePartitureGroupImpl implements DeletePartitureGroup {

    private final PartitureGroupService partitureGroupService;
    private final UserPartitureGroupService userPartitureGroupService;
    
    @Override
    public void execute(final Long partitureGroupId) {
        final List<UserPartitureGroupResponse> users = this.userPartitureGroupService.getUserWithPartitureGroup(partitureGroupId);
        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(users))) {
            throw new BadRequestException("No se pueden eliminar grupos de partituras asociadas a musicos");
        }
        this.partitureGroupService.logicalDelete(partitureGroupId);
    }

}
