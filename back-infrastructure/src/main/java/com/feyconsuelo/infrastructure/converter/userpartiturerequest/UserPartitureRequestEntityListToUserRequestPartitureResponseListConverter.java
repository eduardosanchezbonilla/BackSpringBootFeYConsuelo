package com.feyconsuelo.infrastructure.converter.userpartiturerequest;

import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureResponse;
import com.feyconsuelo.infrastructure.entities.userpartiturerequest.UserPartitureRequestEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserPartitureRequestEntityListToUserRequestPartitureResponseListConverter {

    private final UserPartitureRequestEntityToUserRequestPartitureResponseConverter userPartitureRequestEntityToUserRequestPartitureResponseConverter;

    public List<UserRequestPartitureResponse> convert(final List<UserPartitureRequestEntity> userPartitureRequestEntities) {
        if (CollectionUtils.isEmpty(userPartitureRequestEntities)) {
            return List.of();
        }
        return userPartitureRequestEntities.stream()
                .map(this.userPartitureRequestEntityToUserRequestPartitureResponseConverter::convert)
                .toList();
    }
}
