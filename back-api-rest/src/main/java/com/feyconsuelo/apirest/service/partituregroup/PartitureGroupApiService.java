package com.feyconsuelo.apirest.service.partituregroup;

import com.feyconsuelo.apirest.service.partituregroup.delete.DeletePartitureGroupService;
import com.feyconsuelo.apirest.service.partituregroup.insert.InsertPartitureGroupService;
import com.feyconsuelo.apirest.service.partituregroup.query.GetPartitureGroupService;
import com.feyconsuelo.apirest.service.partituregroup.update.UpdatePartitureGroupService;
import com.feyconsuelo.openapi.api.PartitureGroupControllerApiDelegate;
import com.feyconsuelo.openapi.model.PartitureGroupRequestDto;
import com.feyconsuelo.openapi.model.PartitureGroupResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class PartitureGroupApiService implements PartitureGroupControllerApiDelegate {

    private final DeletePartitureGroupService deletePartitureGroupService;
    private final InsertPartitureGroupService insertPartitureGroupService;
    private final UpdatePartitureGroupService updatePartitureGroupService;
    private final GetPartitureGroupService getPartitureGroupService;

    @Override
    public ResponseEntity<Void> deletePartitureGroup(final Long partitureGroupId) {
        return this.deletePartitureGroupService.deletePartitureGroup(partitureGroupId);
    }

    @Override
    public ResponseEntity<Void> postPartitureGroup(final PartitureGroupRequestDto partitureGroupRequestDto) {
        return this.insertPartitureGroupService.postPartitureGroup(partitureGroupRequestDto);
    }

    @Override
    public ResponseEntity<Void> updatePartitureGroup(final Long partitureGroupId,
                                                     final PartitureGroupRequestDto partitureGroupRequestDto) {
        return this.updatePartitureGroupService.updatePartitureGroup(partitureGroupId, partitureGroupRequestDto);
    }


    @Override
    public ResponseEntity<List<PartitureGroupResponseDto>> getAllPartitureGroups() {
        return this.getPartitureGroupService.getAllPartitureGroups();
    }

    @Override
    public ResponseEntity<PartitureGroupResponseDto> getPartitureGroup(final Long partitureGroupId) {
        return this.getPartitureGroupService.getPartitureGroup(partitureGroupId);
    }

}
