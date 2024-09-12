package com.feyconsuelo.apirest.validate.user;

import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.model.user.UserRoleEnum;
import com.feyconsuelo.openapi.model.UpdateUserRolesDto;
import com.feyconsuelo.openapi.model.UserRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class ValidateUserService {

    public void validate(final UserRequestDto userRequestDto) {
        this.validateRoles(userRequestDto.getRoles());
    }

    public void validate(final UpdateUserRolesDto updateUserRolesDto) {
        this.validateRoles(updateUserRolesDto.getRoles());
    }

    private void validateRoles(final List<String> roles) {
        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(roles))) {
            // los roles deben ser valores de la enumeracion UserRoleEnum
            roles.forEach(role -> {
                try {
                    UserRoleEnum.valueOf(role);
                } catch (final IllegalArgumentException e) {
                    log.error("Role {} not exists", role);
                    throw new BadRequestException("Role " + role + " not exists");
                }
            });
        }
    }
}
