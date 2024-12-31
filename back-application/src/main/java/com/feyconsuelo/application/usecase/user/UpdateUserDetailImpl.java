package com.feyconsuelo.application.usecase.user;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.user.UpdateUserDetailRequest;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.usecase.user.UpdateUserDetail;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UpdateUserDetailImpl implements UpdateUserDetail {

    private final MusicianService musicianService;

    private final UserService userService;

    private final ResizeImageImpl resizeImageService;

    @Value("${default-images.musician}")
    private String defaultMusicianImage;
    
    @Value("${default-images.user}")
    private String defaultUserImage;

    @Override
    public void execute(final UpdateUserDetailRequest updateUserDetailRequest) {

        final Optional<UserResponse> userOptional = this.userService.get(updateUserDetailRequest.getUsername());

        // sino existe el usuario devolvemos error de NotFound
        if (userOptional.isEmpty()) {
            throw new NotFoundException("No existe el usuario que intenta modificar");
        }

        // actualizamos
        this.userService.updateDetail(updateUserDetailRequest.getUsername(), updateUserDetailRequest);

        // si el usuario es musico, actualizamos tambien su informacion (el dni no puede cambiar en la info del usuario)
        final Optional<MusicianResponse> musician = this.musicianService.getByDni(updateUserDetailRequest.getUsername().toUpperCase());

        // si es musico
        if (musician.isPresent()) {
            String musicianImage = "";
            if (StringUtils.isNotEmpty(updateUserDetailRequest.getImage()) &&
                    !updateUserDetailRequest.getImage().equals(this.defaultMusicianImage) &&
                    !updateUserDetailRequest.getImage().equals(this.defaultUserImage)
            ) {
                musicianImage = this.resizeImageService.resizeImage(updateUserDetailRequest.getImage());
            }

            final MusicianRequest musicianRequest = MusicianRequest.builder()
                    .dni(musician.get().getDni())
                    .name(updateUserDetailRequest.getName())
                    .surname(updateUserDetailRequest.getSurname())
                    .direction(updateUserDetailRequest.getDirection())
                    .municipality(updateUserDetailRequest.getMunicipality())
                    .province(updateUserDetailRequest.getProvince())
                    .email(updateUserDetailRequest.getEmail())
                    .voiceId(musician.get().getVoice().getId())
                    .image(musicianImage)
                    .birthDate(musician.get().getBirthDate())
                    .registrationDate(musician.get().getRegistrationDate())
                    .inventoryObservations(musician.get().getInventoryObservations())
                    .build();

            this.musicianService.update(musician.get().getId(), musicianRequest);
        }
    }
}
