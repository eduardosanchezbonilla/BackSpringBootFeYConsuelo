package com.feyconsuelo.apirest.service.user;

import com.feyconsuelo.apirest.service.user.delete.DeleteUserService;
import com.feyconsuelo.apirest.service.user.insert.InsertUserService;
import com.feyconsuelo.apirest.service.user.query.GetUserService;
import com.feyconsuelo.apirest.service.user.update.UpdateUserPasswordService;
import com.feyconsuelo.apirest.service.user.update.UpdateUserRolesService;
import com.feyconsuelo.openapi.api.UserControllerApiDelegate;
import com.feyconsuelo.openapi.model.UpdateUserPasswordDto;
import com.feyconsuelo.openapi.model.UpdateUserRolesDto;
import com.feyconsuelo.openapi.model.UserRequestDto;
import com.feyconsuelo.openapi.model.UserResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserApiService implements UserControllerApiDelegate {

    private final DeleteUserService deleteUserService;
    private final InsertUserService insertUserService;
    private final UpdateUserRolesService updateUserRolesService;
    private final UpdateUserPasswordService updateUserPasswordService;
    private final GetUserService getUserService;

    @Override
    public ResponseEntity<Void> deleteUser(final String username) {
        return this.deleteUserService.deleteUser(username);
    }

    @Override
    public ResponseEntity<Void> postUser(final UserRequestDto userRequestDto) {
        return this.insertUserService.postUser(userRequestDto);
    }

    @Override
    public ResponseEntity<List<UserResponseDto>> getAllUsers() {
        return this.getUserService.getAllUsers();
    }

    @Override
    public ResponseEntity<UserResponseDto> getUser(final String username) {
        return this.getUserService.getUser(username);
    }

    @Override
    public ResponseEntity<Void> updateUserRoles(final String username,
                                                final UpdateUserRolesDto updateUserRolesDto) {
        return this.updateUserRolesService.updateUserRoles(username, updateUserRolesDto);
    }

    @Override
    public ResponseEntity<Void> updateUserPassword(final String username,
                                                   final UpdateUserPasswordDto updateUserPasswordDto) {
        return this.updateUserPasswordService.updateUserPassword(username, updateUserPasswordDto);
    }


}
