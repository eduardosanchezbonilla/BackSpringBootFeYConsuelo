package com.feyconsuelo.apirest.service.user.query;

import com.feyconsuelo.apirest.converter.user.UserResponseListToUserResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.user.UserResponseToUserResponseDtoConverter;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.usecase.user.GetAllUsers;
import com.feyconsuelo.domain.usecase.user.GetUser;
import com.feyconsuelo.openapi.model.UserResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetUserService {

    private final GetAllUsers getAllUsers;

    private final GetUser getUser;

    private final UserResponseListToUserResponseDtoListConverter userResponseListToUserResponseDtoListConverter;

    private final UserResponseToUserResponseDtoConverter userResponseToUserResponseDtoConverter;

    public ResponseEntity<List<UserResponseDto>> getAllUsers() {
        final List<UserResponse> userResponseList = this.getAllUsers.execute();
        if (CollectionUtils.isEmpty(userResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.userResponseListToUserResponseDtoListConverter.convert(userResponseList));
    }

    public ResponseEntity<UserResponseDto> getUser(final String username) {
        final Optional<UserResponseDto> userResponseDto = this.getUser.execute(username.toLowerCase()).map(this.userResponseToUserResponseDtoConverter::convert);
        return userResponseDto.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
