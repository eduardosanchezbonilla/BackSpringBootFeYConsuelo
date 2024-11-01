package com.feyconsuelo.apirest.converter.user;

import com.feyconsuelo.domain.model.user.UserMusicianResponse;
import com.feyconsuelo.openapi.model.UserResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserMusicianResponseListToUserResponseDtoListConverter {

    private final UserMusicianResponseToUserResponseDtoConverter userMusicianResponseToUserResponseDtoConverter;

    public List<UserResponseDto> convert(final List<UserMusicianResponse> userMusicianResponseList) {
        if (CollectionUtils.isEmpty(userMusicianResponseList)) {
            return List.of();
        }
        return userMusicianResponseList.stream()
                .map(this.userMusicianResponseToUserResponseDtoConverter::convert)
                .sorted(Comparator.comparing(UserResponseDto::getUsername))
                .toList();
    }

}
