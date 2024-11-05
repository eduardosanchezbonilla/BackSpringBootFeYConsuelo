package com.feyconsuelo.application.service.user;

import com.feyconsuelo.domain.model.user.UpdateUserDetailRequest;
import com.feyconsuelo.domain.model.user.UserMusicianResponse;
import com.feyconsuelo.domain.model.user.UserRequest;
import com.feyconsuelo.domain.model.user.UserResponse;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

public interface UserService {

    void delete(String username);

    void logicalDelete(String username);

    List<UserResponse> getAll();

    Optional<UserResponse> get(String username);

    void insert(UserRequest userRequest);

    void updateRoles(String username, List<String> roles);

    void updatePassword(String username, String password, Boolean expiredPassword);

    void updateDetail(String username, UpdateUserDetailRequest updateUserDetailRequest);

    void updateFirebaseToken(String username, List<String> firebaseToken);

    void updateLastAccessDate(String username, LocalDateTime lassDateAccess);

    List<UserMusicianResponse> getAllWithMusicianData();

}
