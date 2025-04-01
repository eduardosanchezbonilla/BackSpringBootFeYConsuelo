package com.feyconsuelo.application.service.contractgroup;

import com.feyconsuelo.domain.model.contractgroup.ContractGroupRequest;
import com.feyconsuelo.domain.model.contractgroup.ContractGroupResponse;

import java.util.List;
import java.util.Optional;

public interface ContractGroupService {

    void delete(Long contractGroupId);

    void logicalDelete(Long contractGroupId);

    List<ContractGroupResponse> getAll(final List<Long> contractGroupIdList,
                                       final Boolean allContractGroups);

    Optional<ContractGroupResponse> get(Long contractGroupId);

    void insert(ContractGroupRequest contractGroupRequest);

    void update(Long contractGroupId, ContractGroupRequest contractGroupRequest);

}
