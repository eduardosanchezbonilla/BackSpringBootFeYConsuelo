package com.feyconsuelo.application.usecase.contractgroup;

import com.feyconsuelo.application.service.contractgroup.ContractGroupService;
import com.feyconsuelo.domain.model.contractgroup.ContractGroupResponse;
import com.feyconsuelo.domain.usecase.contractgroup.GetAllContractGroups;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetAllContractGroupsImpl implements GetAllContractGroups {

    private final ContractGroupService contractGroupService;

    @Override
    public List<ContractGroupResponse> execute() {

        return this.contractGroupService.getAll(List.of(), Boolean.TRUE);
    }
}
