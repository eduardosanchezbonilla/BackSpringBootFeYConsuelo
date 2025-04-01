package com.feyconsuelo.apirest.service.contractgroup;

import com.feyconsuelo.apirest.service.contractgroup.delete.DeleteContractGroupService;
import com.feyconsuelo.apirest.service.contractgroup.insert.InsertContractGroupService;
import com.feyconsuelo.apirest.service.contractgroup.query.GetContractGroupService;
import com.feyconsuelo.apirest.service.contractgroup.update.UpdateContractGroupService;
import com.feyconsuelo.openapi.api.ContractGroupControllerApiDelegate;
import com.feyconsuelo.openapi.model.ContractGroupRequestDto;
import com.feyconsuelo.openapi.model.ContractGroupResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class ContractGroupApiService implements ContractGroupControllerApiDelegate {

    private final DeleteContractGroupService deleteContractGroupService;
    private final InsertContractGroupService insertContractGroupService;
    private final UpdateContractGroupService updateContractGroupService;
    private final GetContractGroupService getContractGroupService;

    @Override
    public ResponseEntity<Void> deleteContractGroup(final Long contractGroupId) {
        return this.deleteContractGroupService.deleteContractGroup(contractGroupId);
    }

    @Override
    public ResponseEntity<Void> postContractGroup(final ContractGroupRequestDto contractGroupRequestDto) {
        return this.insertContractGroupService.postContractGroup(contractGroupRequestDto);
    }

    @Override
    public ResponseEntity<Void> updateContractGroup(final Long contractGroupId,
                                                    final ContractGroupRequestDto contractGroupRequestDto) {
        return this.updateContractGroupService.updateContractGroup(contractGroupId, contractGroupRequestDto);
    }


    @Override
    public ResponseEntity<List<ContractGroupResponseDto>> getAllContractGroups() {
        return this.getContractGroupService.getAllContractGroups();
    }

    @Override
    public ResponseEntity<ContractGroupResponseDto> getContractGroup(final Long contractGroupId) {
        return this.getContractGroupService.getContractGroup(contractGroupId);
    }

}
