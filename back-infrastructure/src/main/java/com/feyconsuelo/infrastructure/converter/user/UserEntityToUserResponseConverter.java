package com.feyconsuelo.infrastructure.converter.user;

import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserEntityToUserResponseConverter {

    public UserResponse convert(final UserEntity userEntity) {
        return UserResponse.builder()
                .username(userEntity.getUsername())
                .password(userEntity.getPassword())
                .roles(
                        CollectionUtils.isEmpty(userEntity.getRoles()) ?
                                List.of() :
                                userEntity.getRoles().stream()
                                        .map(role -> role.getId().getRole())
                                        .toList()
                )
                .deletedDate(userEntity.getDeleteDate())
                .passwordExpired(userEntity.getPasswordExpired())
                .dni(userEntity.getDni())
                .name(userEntity.getName())
                .surname(userEntity.getSurname())
                .direction(userEntity.getDirection())
                .municipality(userEntity.getMunicipality())
                .province(userEntity.getProvince())
                .email(userEntity.getEmail())
                .description(userEntity.getDescription())
                .image(userEntity.getImage())
                .build();
    }

}
