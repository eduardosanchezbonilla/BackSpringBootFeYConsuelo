package com.feyconsuelo.infrastructure.converter.user;

import com.feyconsuelo.domain.model.user.UserRequest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.util.CollectionUtils;

import java.util.List;

@ExtendWith(MockitoExtension.class)
class UserListToUserRequestEntityListConverterTest {

    @Mock
    private UserRequestToUserEntityConverter userRequestToUserEntityConverter;
    @InjectMocks
    private UserRequestListToUserEntityListConverter userRequestListToUserEntityListConverter;

    @Test
    void convertEmptyList() {
        // call
        Assertions.assertTrue(CollectionUtils.isEmpty(this.userRequestListToUserEntityListConverter.convert(List.of())));
    }

    @Test
    void convertNotEmptyList() {
        // call
        Assertions.assertFalse(CollectionUtils.isEmpty(this.userRequestListToUserEntityListConverter.convert(List.of(UserRequest.builder().build()))));
    }

}