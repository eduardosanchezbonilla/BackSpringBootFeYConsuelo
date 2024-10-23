package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.application.usecase.user.GetUserImpl;
import com.feyconsuelo.application.usecase.user.InsertUserImpl;
import com.feyconsuelo.application.usecase.user.UpdateUserRolesImpl;
import com.feyconsuelo.application.usecase.voice.GetVoiceImpl;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.user.UpdateUserRolesRequest;
import com.feyconsuelo.domain.model.user.UserRequest;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.model.user.UserRoleEnum;
import com.feyconsuelo.domain.usecase.musician.InsertMusician;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class InsertMusicianImpl implements InsertMusician {

    private final MusicianService musicianService;

    private final GetUserImpl getUser;

    private final InsertUserImpl insertUser;

    private final UpdateUserRolesImpl updateUserRoles;

    private final GetVoiceImpl getVoice;

    private final ResizeImageImpl resizeImageService;

    @Value("${default-images.musician}")
    private String defaultMusicianImage;

    @Override
    public MusicianResponse execute(final MusicianRequest musicianRequest) {

        // si ya existe un musico con ese dni, lanzamos error
        final Optional<MusicianResponse> findMusician = this.musicianService.getByDni(musicianRequest.getDni());

        if (findMusician.isPresent()) {
            throw new BadRequestException("Ya existe otro músico con el DNI que estás introduciendo");
        }

        // comprobamos si la voz que estan pasando existe, sino devolvemos error
        final var voice = this.getVoice.execute(musicianRequest.getVoiceId());

        if (voice.isEmpty()) {
            throw new BadRequestException("La voz inatroducida no existe");
        }

        // si estan enviando imagen y no es la imagen por defecto, debemos redimensionarla
        if (StringUtils.isNotEmpty(musicianRequest.getImage()) && !musicianRequest.getImage().equals(this.defaultMusicianImage)) {
            musicianRequest.setImage(this.resizeImageService.resizeImage(musicianRequest.getImage()));
        }

        // insertamos el musico
        final MusicianResponse insertedMusicianRequest = this.musicianService.insert(musicianRequest);

        // si la insercion del musico ha ido bien, entonces vamos a crear un usuario asociado a este musico
        this.createUserAssociatedToMusician(insertedMusicianRequest.getDni());

        return insertedMusicianRequest;
    }

    public void createUserAssociatedToMusician(final String dni) {

        final String username = dni.toLowerCase();
        final Optional<UserResponse> user = this.getUser.execute(username);

        if (user.isEmpty()) {
            // sino existe lo creo
            this.insertUser.execute(
                    UserRequest.builder()
                            .username(username)
                            .password(username)
                            .roles(List.of(UserRoleEnum.MUSICO.getId()))
                            .passwordExpired(Boolean.TRUE)
                            .build()
            );
        } else {
            // si existe lo dejo solo con rol de musico
            this.updateUserRoles.execute(
                    UpdateUserRolesRequest.builder()
                            .username(username)
                            .roles(List.of(UserRoleEnum.MUSICO.getId()))
                            .build()
            );
        }
    }
}
