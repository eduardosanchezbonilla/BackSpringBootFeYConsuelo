package com.feyconsuelo.infrastructure.converter.userpartiturerequest;

import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureRequest;
import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import com.feyconsuelo.infrastructure.entities.userpartiturerequest.UserPartitureRequestEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserRequestPartitureRequestToUserPartitureRequestEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public UserPartitureRequestEntity convert(final UserRequestPartitureRequest userRequestPartitureRequest,
                                              final UserEntity userEntity
    ) {
        return UserPartitureRequestEntity.builder()
                .user(userEntity)
                .partitureRequest(userRequestPartitureRequest.getDescription())
                .readed(userRequestPartitureRequest.getReaded())
                .updateUserPartitureRequest(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public UserPartitureRequestEntity updateEntity(final UserPartitureRequestEntity userPartitureRequestEntity,
                                                   final UserRequestPartitureRequest userRequestPartitureRequest,
                                                   final UserEntity userEntity
    ) {
        userPartitureRequestEntity.setUser(userEntity);
        userPartitureRequestEntity.setPartitureRequest(userRequestPartitureRequest.getDescription());
        userPartitureRequestEntity.setReaded(userRequestPartitureRequest.getReaded());
        userPartitureRequestEntity.setUpdateUserPartitureRequest(this.tokenInfoExtractorService.getUsername());

        return userPartitureRequestEntity;
    }

}
