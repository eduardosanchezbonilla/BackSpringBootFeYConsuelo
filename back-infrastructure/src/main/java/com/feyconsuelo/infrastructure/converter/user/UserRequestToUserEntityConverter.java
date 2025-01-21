package com.feyconsuelo.infrastructure.converter.user;

import com.feyconsuelo.domain.model.user.UserRequest;
import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import com.feyconsuelo.infrastructure.entities.user.UserRoleEntity;
import com.feyconsuelo.infrastructure.entities.user.UserRolePK;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserRequestToUserEntityConverter {

    @Value("${default-images.musician}")
    private String defaultMusicianImage;

    @Value("${default-images.user}")
    private String defaultUserImage;

    private String getUserImage(final String image) {
        if (StringUtils.isEmpty(image)) {
            return null;
        } else {
            if (image.equals(this.defaultMusicianImage) || image.equals(this.defaultUserImage)) {
                return null;
            } else {
                return image;
            }
        }
    }

    public UserEntity convert(final UserRequest userRequest) {
        return UserEntity.builder()
                .username(userRequest.getUsername())
                .password(userRequest.getPassword())
                .roles(CollectionUtils.isEmpty(userRequest.getRoles()) ?
                        List.of() :
                        userRequest.getRoles().stream()
                                .map(role -> UserRoleEntity.builder()
                                        .id(UserRolePK.builder()
                                                .username(userRequest.getUsername())
                                                .role(role)
                                                .build()
                                        )
                                        .deleteDate(userRequest.getDeletedDate())
                                        .build()
                                )
                                .toList()
                )
                .deleteDate(userRequest.getDeletedDate())
                .passwordExpired(userRequest.getPasswordExpired() == null ? Boolean.FALSE : userRequest.getPasswordExpired())
                .dni(userRequest.getDni())
                .name(userRequest.getName())
                .surname(userRequest.getSurname())
                .direction(userRequest.getDirection())
                .municipality(userRequest.getMunicipality())
                .province(userRequest.getProvince())
                .email(userRequest.getEmail())
                .description(userRequest.getDescription())
                .image(this.getUserImage(userRequest.getImage()))
                .imageThumbnail(this.getUserImage(userRequest.getImageThumbnail()))
                .phoneNumber(userRequest.getPhoneNumber())
                .build();
    }

}
