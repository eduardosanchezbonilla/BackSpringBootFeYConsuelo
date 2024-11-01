package com.feyconsuelo.apirest.service.userpartiturerequest.query;

import com.feyconsuelo.apirest.converter.partituregroup.PartitureGroupResponseListToPartitureGroupResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.userpratiturerequest.UserRequestPartitureGroupByUserResponseListToUserRequestPartitureGroupByUserResponseDtoListConverter;
import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureGroupByUserResponse;
import com.feyconsuelo.domain.usecase.userpartiturerequest.GetAllRequestPartitureGroupByUser;
import com.feyconsuelo.openapi.model.UserRequestPartitureGroupByUserResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetRequestPartitureService {

    private final GetAllRequestPartitureGroupByUser getAllRequestPartitureGroupByUser;

    private final UserRequestPartitureGroupByUserResponseListToUserRequestPartitureGroupByUserResponseDtoListConverter userRequestPartitureGroupByUserResponseListToUserRequestPartitureGroupByUserResponseDtoListConverter;

    private final PartitureGroupResponseListToPartitureGroupResponseDtoListConverter partitureGroupResponseListToPartitureGroupResponseDtoListConverter;

    public ResponseEntity<List<UserRequestPartitureGroupByUserResponseDto>> getAllRequestPartitureGroupByUser(final Boolean all) {
        final List<UserRequestPartitureGroupByUserResponse> userRequestPartitureGroupByUserResponseList = this.getAllRequestPartitureGroupByUser.execute(all);
        if (CollectionUtils.isEmpty(userRequestPartitureGroupByUserResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.userRequestPartitureGroupByUserResponseListToUserRequestPartitureGroupByUserResponseDtoListConverter.convert(userRequestPartitureGroupByUserResponseList));
    }

}
