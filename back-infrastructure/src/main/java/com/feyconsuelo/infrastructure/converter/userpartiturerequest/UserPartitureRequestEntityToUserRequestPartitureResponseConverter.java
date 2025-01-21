package com.feyconsuelo.infrastructure.converter.userpartiturerequest;

import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureResponse;
import com.feyconsuelo.infrastructure.converter.user.UserEntityToUserResponseConverter;
import com.feyconsuelo.infrastructure.entities.userpartiturerequest.UserPartitureRequestEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserPartitureRequestEntityToUserRequestPartitureResponseConverter {

    private final UserEntityToUserResponseConverter userEntityToUserResponseConverter;

    public UserRequestPartitureResponse convert(final UserPartitureRequestEntity userPartitureRequestEntity) {
        return UserRequestPartitureResponse.builder()
                .id(userPartitureRequestEntity.getId())
                .user(this.userEntityToUserResponseConverter.convert(userPartitureRequestEntity.getUser(), Boolean.TRUE))
                .description(userPartitureRequestEntity.getPartitureRequest())
                .readed(userPartitureRequestEntity.getReaded())
                .deleteDate(userPartitureRequestEntity.getDeleteDatePartitureRequest())
                .build();
    }

}
