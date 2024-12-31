package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.application.usecase.voice.GetVoiceImpl;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.user.UpdateUserDetailRequest;
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
    
    @Value("${default-images.musician}")
    private String defaultMusicianImage;

    @Override
    public MusicianResponse execute(final Long musicianId, final MusicianRequest musicianRequest) {

        final Optional<MusicianResponse> musicianOptional = this.musicianService.get(musicianId);

        if (musicianOptional.isEmpty()) {
            throw new NotFoundException("No existe el músico que desea actualizar");
        }

        // si ya existe un musico con ese dni, lanzamos error
        final Optional<MusicianResponse> findMusician = this.musicianService.getByDni(musicianRequest.getDni());

        if (findMusician.isPresent() && Boolean.FALSE.equals(musicianId.equals(findMusician.get().getId()))) {
            throw new BadRequestException("Ya existe otro músico con el DNI que estás introduciendo");
        }

        // comprobamos si la voz que estan pasando existe, sino devolvemos error
        final var voice = this.getVoice.execute(musicianRequest.getVoiceId());

        if (voice.isEmpty()) {
            throw new BadRequestException("La voz introducida no existe");
        }

        // si estan enviando imagen, debemos redimensionarla
        if (StringUtils.isNotEmpty(musicianRequest.getImage()) && !musicianRequest.getImage().equals(this.defaultMusicianImage)) {
            musicianRequest.setImage(this.resizeImageService.resizeImage(musicianRequest.getImage()));
        }

        final String oldDni = musicianOptional.get().getDni();
        final MusicianResponse updatedMusicianRequest = this.musicianService.update(musicianId, musicianRequest);

        // si ha cambiado el DNI, entonces tenemos que borrar el usuario asociado al antiguo DNI, y crear el usuario asociado al nuevo DNI
        this.updateUserAssociatedToMusician(oldDni, updatedMusicianRequest.getDni(), updatedMusicianRequest);

        return updatedMusicianRequest;

    }

    private void updateUserAssociatedToMusician(final String oldDni, final String newDni, final MusicianResponse musicianResponse) {
        final String oldUsername = oldDni.toLowerCase();
        final String newUsername = newDni.toLowerCase();

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
                    .description(musicianResponse.getInventoryObservations())
                    .image(musicianResponse.getImage())
                    .build();
            // actualizamos
            this.userService.updateDetail(newUsername, updateUserDetailRequest);
        } else {
            // si ha cambiado
            // borramos el usuario asociado al antiguo DNI
            this.deleteMusician.deleteUserAssociatedToMusician(oldUsername);

            // insertamos el nuevo usuario
            this.insertMusician.createUserAssociatedToMusician(newUsername);
        }
    }

}
