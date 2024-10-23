package com.feyconsuelo.application.usecase.user;

import com.feyconsuelo.application.service.security.PasswordEncoderService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.model.user.UserRequest;
import com.feyconsuelo.domain.usecase.user.InsertUser;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertUserImpl implements InsertUser {

    private final UserService userService;
    private final PasswordEncoderService passwordEncoderService;
    private final ResizeImageImpl resizeImageService;

    @Override
    public void execute(final UserRequest userRequest) {

        // si ya existe un usuario con ese username devolvemos error de BadRequest
        this.userService.get(userRequest.getUsername())
                .ifPresent(existUser -> {
                            throw new BadRequestException("Ya existe el usuario que intenta insertar");
                        }
                );

        this.userService.insert(
                UserRequest.builder()
                        .username(userRequest.getUsername())
                        .password(this.passwordEncoderService.encodePassword(userRequest.getPassword()))
                        .roles(userRequest.getRoles())
                        .passwordExpired(userRequest.getPasswordExpired())
                        .dni(userRequest.getDni())
                        .name(userRequest.getName())
                        .surname(userRequest.getSurname())
                        .direction(userRequest.getDirection())
                        .municipality(userRequest.getMunicipality())
                        .province(userRequest.getProvince())
                        .email(userRequest.getEmail())
                        .description(userRequest.getDescription())
                        .image(StringUtils.isEmpty(userRequest.getImage()) ? userRequest.getImage() : this.resizeImageService.resizeImage(userRequest.getImage()))
                        .build()
        );
    }
}
