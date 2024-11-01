package com.feyconsuelo.apirest.converter.userpratiturerequest;

import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureResponse;
import com.feyconsuelo.openapi.model.UserRequestPartitureResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserRequestPartitureResponseListToUserRequestPartitureResponseDtoListConverter {

    private final UserRequestPartitureResponseToUserRequestPartitureResponseDtoConverter userRequestPartitureResponseToUserRequestPartitureResponseDtoConverter;

    public List<UserRequestPartitureResponseDto> convert(final List<UserRequestPartitureResponse> userRequestPartitureResponseList) {
        if (CollectionUtils.isEmpty(userRequestPartitureResponseList)) {
            return List.of();
        }
        return userRequestPartitureResponseList.stream()
                .map(this.userRequestPartitureResponseToUserRequestPartitureResponseDtoConverter::convert)
                .sorted(Comparator.comparing(UserRequestPartitureResponseDto::getId))
                .toList();
    }

}
