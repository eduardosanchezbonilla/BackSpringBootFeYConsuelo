package com.feyconsuelo.infrastructure.service.user;

import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.user.UpdateUserDetailRequest;
import com.feyconsuelo.domain.model.user.UserMusicianResponse;
import com.feyconsuelo.domain.model.user.UserRequest;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.infrastructure.converter.user.UserEntityListToUserResponseListConverter;
import com.feyconsuelo.infrastructure.converter.user.UserEntityToUserResponseConverter;
import com.feyconsuelo.infrastructure.converter.user.UserMusicianEntityListToUserMusicianResponseListConverter;
import com.feyconsuelo.infrastructure.converter.user.UserRequestToUserEntityConverter;
import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import com.feyconsuelo.infrastructure.entities.user.UserMusicianEntity;
import com.feyconsuelo.infrastructure.entities.user.UserRoleEntity;
import com.feyconsuelo.infrastructure.entities.user.UserRolePK;
import com.feyconsuelo.infrastructure.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserServiceImpl implements UserService {

    private final UserRepository userRepository;
    private final UserRequestToUserEntityConverter userRequestToUserEntityConverter;
    private final UserEntityListToUserResponseListConverter userEntityListToUserResponseListConverter;
    private final UserEntityToUserResponseConverter userEntityToUserResponseConverter;
    private final UserMusicianEntityListToUserMusicianResponseListConverter userMusicianEntityListToUserMusicianResponseListConverter;

    @Value("${default-images.user}")
    private String defaultUserImage;

    @Override
    public void delete(final String username) {
        this.userRepository.deleteById(username);
    }

    @Override
    public void logicalDelete(final String username) {

        final var user = this.userRepository.findUserActiveByUserName(username);

        if (user.isEmpty()) {
            throw new NotFoundException("No existe el usuario que desea eliminar");
        }

        user.get().setDeleteDate(LocalDateTime.now());
        user.get().getRoles().forEach(role -> role.setDeleteDate(LocalDateTime.now()));
        this.userRepository.save(user.get());
    }

    @Override
    public List<UserResponse> getAll() {
        final List<UserEntity> users = this.userRepository.findAllActives();
        return this.userEntityListToUserResponseListConverter.convert(users);
    }

    @Override
    public Optional<UserResponse> get(final String username) {
        final var user = this.userRepository.findUserActiveByUserName(username);
        return user.map(this.userEntityToUserResponseConverter::convert);
    }

    @Override
    public void insert(final UserRequest userRequest) {
        this.userRepository.save(
                this.userRequestToUserEntityConverter.convert(userRequest)
        );
    }

    @Override
    public void updateRoles(final String username,
                            final List<String> roles) {

        final var user = this.userRepository.findUserActiveByUserName(username);

        if (user.isEmpty()) {
            throw new NotFoundException("No existe el usuario al que quiere modificar los roles");
        }

        final List<UserRoleEntity> currentRoles = user.get().getRoles();
        currentRoles.clear();
        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(roles))) {
            currentRoles.addAll(
                    roles.stream()
                            .map(role -> UserRoleEntity.builder()
                                    .id(UserRolePK.builder()
                                            .username(username)
                                            .role(role)
                                            .build()
                                    )
                                    .build()
                            )
                            .toList()
            );
        }

        this.userRepository.save(user.get());
    }

    @Override
    public void updatePassword(final String username,
                               final String password) {

        final var user = this.userRepository.findUserActiveByUserName(username);

        if (user.isEmpty()) {
            throw new NotFoundException("No existe el usuario al que desea cambaiar el password");
        }

        user.get().setPassword(password);
        user.get().setPasswordExpired(Boolean.FALSE); // cuando actualizamos el password, ya no puede estar expirado
        this.userRepository.save(user.get());
    }

    @Override
    public void updateDetail(final String username,
                             final UpdateUserDetailRequest updateUserDetailRequest
    ) {

        final var user = this.userRepository.findUserActiveByUserName(username);

        if (user.isEmpty()) {
            throw new NotFoundException("No existe el usuario que desea modificar");
        }

        user.get().setDni(updateUserDetailRequest.getDni());
        user.get().setName(updateUserDetailRequest.getName());
        user.get().setSurname(updateUserDetailRequest.getSurname());
        user.get().setDirection(updateUserDetailRequest.getDirection());
        user.get().setMunicipality(updateUserDetailRequest.getMunicipality());
        user.get().setProvince(updateUserDetailRequest.getProvince());
        user.get().setEmail(updateUserDetailRequest.getEmail());
        user.get().setDescription(updateUserDetailRequest.getDescription());
        user.get().setImage(this.getUserIaage(updateUserDetailRequest));
        this.userRepository.save(user.get());
    }

    private String getUserIaage(final UpdateUserDetailRequest updateUserDetailRequest) {
        if (StringUtils.isEmpty(updateUserDetailRequest.getImage())) {
            return null;
        } else {
            if (updateUserDetailRequest.getImage().equals(this.defaultUserImage)) {
                return null;
            } else {
                return updateUserDetailRequest.getImage();
            }
        }
    }

    @Override
    public void updateFirebaseToken(final String username,
                                    final List<String> firebaseToken) {

        final var user = this.userRepository.findUserActiveByUserName(username);

        if (user.isEmpty()) {
            throw new NotFoundException("No existe el usuario al que desea cambaiar el password");
        }

        user.get().setFirebaseToken(firebaseToken);
        this.userRepository.save(user.get());
    }

    @Override
    public List<UserMusicianResponse> getAllWithMusicianData() {
        final List<UserMusicianEntity> users = this.userRepository.findAllActivesWithMusicianData();
        return this.userMusicianEntityListToUserMusicianResponseListConverter.convert(users);
    }

}
