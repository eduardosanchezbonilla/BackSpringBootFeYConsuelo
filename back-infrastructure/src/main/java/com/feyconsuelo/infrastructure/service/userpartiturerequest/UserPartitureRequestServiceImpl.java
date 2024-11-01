package com.feyconsuelo.infrastructure.service.userpartiturerequest;

import com.feyconsuelo.application.service.userpartiturerequest.UserPartitureRequestService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureRequest;
import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureResponse;
import com.feyconsuelo.infrastructure.converter.userpartiturerequest.UserPartitureRequestEntityListToUserRequestPartitureResponseListConverter;
import com.feyconsuelo.infrastructure.converter.userpartiturerequest.UserRequestPartitureRequestToUserPartitureRequestEntityConverter;
import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import com.feyconsuelo.infrastructure.entities.userpartiturerequest.UserPartitureRequestEntity;
import com.feyconsuelo.infrastructure.repository.UserPartitureRequestRepository;
import com.feyconsuelo.infrastructure.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserPartitureRequestServiceImpl implements UserPartitureRequestService {
    private final UserRepository userRepository;
    private final UserPartitureRequestRepository userPartitureRequestRepository;
    private final UserRequestPartitureRequestToUserPartitureRequestEntityConverter userRequestPartitureRequestToUserPartitureRequestEntityConverter;
    private final UserPartitureRequestEntityListToUserRequestPartitureResponseListConverter userPartitureRequestEntityListToUserRequestPartitureResponseListConverter;

    @Override
    public void insert(final UserRequestPartitureRequest userRequestPartitureRequest) {
        // obtenemos el usuario
        final Optional<UserEntity> userEntity = this.userRepository.findUserActiveByUserName(userRequestPartitureRequest.getUsername());

        if (userEntity.isEmpty()) {
            throw new NotFoundException("No existe el usuario que intenta registrar la solicitud");
        }

        this.userPartitureRequestRepository.save(
                this.userRequestPartitureRequestToUserPartitureRequestEntityConverter.convert(userRequestPartitureRequest, userEntity.get())
        );
    }

    @Override
    public void logicalDelete(final UserRequestPartitureRequest userRequestPartitureRequest) {

        final var userPartitureEntity = this.userPartitureRequestRepository.findActiveById(userRequestPartitureRequest.getId());

        if (userPartitureEntity.isEmpty()) {
            throw new NotFoundException("No existe el registro que intenta eliminar");
        }

        userPartitureEntity.get().setDeleteDatePartitureRequest(LocalDateTime.now());
        this.userPartitureRequestRepository.save(userPartitureEntity.get());
    }

    @Override
    public List<UserRequestPartitureResponse> getAllUserPartitureRequest() {
        final List<UserPartitureRequestEntity> userPartitureRequest = this.userPartitureRequestRepository.findAllActives();
        return this.userPartitureRequestEntityListToUserRequestPartitureResponseListConverter.convert(userPartitureRequest);
    }

    @Override
    public List<UserRequestPartitureResponse> getAllUserPartitureRequestByUser(final String username) {
        final List<UserPartitureRequestEntity> userPartitureRequest = this.userPartitureRequestRepository.findAllActivesByUser(username);
        return this.userPartitureRequestEntityListToUserRequestPartitureResponseListConverter.convert(userPartitureRequest);
    }

    @Override
    public void markReadUnread(final UserRequestPartitureRequest userRequestPartitureRequest) {
        final var userPartitureEntity = this.userPartitureRequestRepository.findActiveById(userRequestPartitureRequest.getId());

        if (userPartitureEntity.isEmpty()) {
            throw new NotFoundException("No existe el registro que intenta eliminar");
        }

        // obtenemos el usuario
        final Optional<UserEntity> userEntity = this.userRepository.findUserActiveByUserName(userRequestPartitureRequest.getUsername());

        if (userEntity.isEmpty()) {
            throw new NotFoundException("No existe el usuario que intenta registrar la solicitud");
        }

        this.userPartitureRequestRepository.save(
                this.userRequestPartitureRequestToUserPartitureRequestEntityConverter.updateEntity(
                        userPartitureEntity.get(),
                        userRequestPartitureRequest,
                        userEntity.get()
                )
        );
    }

}
