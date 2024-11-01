package com.feyconsuelo.apirest.converter.userpratiturerequest;

import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureResponse;
import com.feyconsuelo.openapi.model.UserRequestPartitureResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserRequestPartitureResponseToUserRequestPartitureResponseDtoConverter {

    public UserRequestPartitureResponseDto convert(final UserRequestPartitureResponse userRequestPartitureResponse) {
        return UserRequestPartitureResponseDto.builder()
                .id(userRequestPartitureResponse.getId())
                .username(userRequestPartitureResponse.getUser().getUsername())
                .description(userRequestPartitureResponse.getDescription())
                .readed(userRequestPartitureResponse.getReaded())
                .build();
    }

}
