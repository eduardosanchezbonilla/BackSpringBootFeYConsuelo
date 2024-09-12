package com.feyconsuelo.infrastructure.converter.user;

import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.util.CollectionUtils;

import java.util.List;

@ExtendWith(MockitoExtension.class)
class UserEntityListToUserRequestListConverterTest {

    @Mock
    private UserEntityToUserResponseConverter userEntityToUserResponseConverter;
    @InjectMocks
    private UserEntityListToUserResponseListConverter userEntityListToUserResponseListConverter;

    @Test
    void convertEmptyList() {
        // call
        Assertions.assertTrue(CollectionUtils.isEmpty(this.userEntityListToUserResponseListConverter.convert(List.of())));
    }

    @Test
    void convertNotEmptyList() {
        // call
        Assertions.assertFalse(CollectionUtils.isEmpty(this.userEntityListToUserResponseListConverter.convert(List.of(UserEntity.builder().build()))));
    }
}