package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.application.usecase.user.GetUserImpl;
import com.feyconsuelo.application.usecase.voice.GetVoiceImpl;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.user.UpdateUserDetailRequest;
import com.feyconsuelo.domain.model.user.UserRequest;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.usecase.musician.UpdateMusician;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UpdateMusicianImpl implements UpdateMusician {

    private final MusicianService musicianService;

    private final InsertMusicianImpl insertMusician;

    private final DeleteMusicianImpl deleteMusician;

    private final GetVoiceImpl getVoice;

    private final ResizeImageImpl resizeImageService;

    private final UserService userService;

    private final GetUserImpl getUser;

    @Value("${default-images.musician}")
    private String defaultMusicianImage;

    @Value("${default-images.user}")
    private String defaultUserImage;

    @Override
    public MusicianResponse execute(final Long musicianId, final MusicianRequest musicianRequest) {

        final Optional<MusicianResponse> musicianThumbnailImage = this.musicianService.get(musicianId, true);
        final Optional<MusicianResponse> musicianOriginalImage = this.musicianService.get(musicianId, false);

        if (musicianThumbnailImage.isEmpty() || musicianOriginalImage.isEmpty()) {
            throw new NotFoundException("No existe el músico que desea actualizar");
        }

        // si ya existe un musico con ese dni, lanzamos error
        final Optional<MusicianResponse> findMusician = this.musicianService.getByDni(musicianRequest.getDni(), true);

        if (findMusician.isPresent() && Boolean.FALSE.equals(musicianId.equals(findMusician.get().getId()))) {
            throw new BadRequestException("Ya existe otro músico con el DNI que estás introduciendo");
        }

        // comprobamos si la voz que estan pasando existe, sino devolvemos error
        final var voice = this.getVoice.execute(musicianRequest.getVoiceId());

        if (voice.isEmpty()) {
            throw new BadRequestException("La voz introducida no existe");
        }

        // si la imagen que viene es igual que el thumbnail, no la guardamos
        // musicianRequest.getImage(), trae el thumbnail (pq es el que devolvimos en el listado)
        // musicianThumbnailImage.getImage(), tiene la imagen thumbnail (pq hemnos pasado true)
        if (musicianRequest.getImage() != null && musicianRequest.getImage().equals(musicianThumbnailImage.get().getImage())) {
            musicianRequest.setImage(musicianOriginalImage.get().getImage());
        }

        // si estan enviando imagen, debemos redimensionarla
        if (StringUtils.isNotEmpty(musicianRequest.getImage()) &&
                !musicianRequest.getImage().equals(this.defaultMusicianImage) &&
                !musicianRequest.getImage().equals(this.defaultUserImage)
        ) {
            musicianRequest.setImageThumbnail(this.resizeImageService.resizeImage(musicianRequest.getImage()));
        }

        final String oldDni = musicianThumbnailImage.get().getDni();
        final MusicianResponse updatedMusicianRequest = this.musicianService.update(musicianId, musicianRequest);

        // si ha cambiado el DNI, entonces tenemos que borrar el usuario asociado al antiguo DNI, y crear el usuario asociado al nuevo DNI
        this.updateUserAssociatedToMusician(oldDni, updatedMusicianRequest.getDni(), updatedMusicianRequest, musicianRequest);

        return updatedMusicianRequest;

    }

    private void updateUserAssociatedToMusician(final String oldDni, final String newDni, final MusicianResponse musicianResponse, final MusicianRequest musicianRequest) {
        final String oldUsername = oldDni.toLowerCase();
        final String newUsername = newDni.toLowerCase();

        // si ha cambiado
        String descriptionOldUser = null;
        final Optional<UserResponse> oldUser = this.getUser.execute(oldUsername);
        if (oldUser.isPresent()) {
            descriptionOldUser = oldUser.get().getDescription();
        }

        // sino ha cambiado, actualizamos solo la informacion deu usuario
        if (oldUsername.equals(newUsername)) {
            final UpdateUserDetailRequest updateUserDetailRequest = UpdateUserDetailRequest.builder()
                    .username(newUsername)
                    .dni(newDni)
                    .name(musicianResponse.getName())
                    .surname(musicianResponse.getSurname())
                    .direction(musicianResponse.getDirection())
                    .municipality(musicianResponse.getMunicipality())
                    .province(musicianResponse.getProvince())
                    .email(musicianResponse.getEmail())
                    .description(descriptionOldUser)
                    .image(musicianRequest.getImage())
                    .imageThumbnail(musicianResponse.getImage())
                    .phoneNumber(musicianResponse.getPhoneNumber())
                    .build();
            // actualizamos
            this.userService.updateDetail(newUsername, updateUserDetailRequest);
        } else {
            // borramos el usuario asociado al antiguo DNI
            this.deleteMusician.deleteUserAssociatedToMusician(oldUsername);

            // insertamos el nuevo usuario
            this.insertMusician.createUserAssociatedToMusician(
                    newUsername,
                    UserRequest.builder()
                            .name(musicianResponse.getName())
                            .dni(musicianResponse.getDni())
                            .direction(musicianResponse.getDirection())
                            .surname(musicianResponse.getSurname())
                            .province(musicianResponse.getProvince())
                            .municipality(musicianResponse.getMunicipality())
                            .description(descriptionOldUser)
                            .email(musicianResponse.getEmail())
                            .image(musicianRequest.getImage())
                            .imageThumbnail(musicianResponse.getImage())
                            .phoneNumber(musicianResponse.getPhoneNumber())
                            .build()
            );
        }
    }

}
