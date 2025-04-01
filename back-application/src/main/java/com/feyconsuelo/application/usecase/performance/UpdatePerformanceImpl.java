package com.feyconsuelo.application.usecase.performance;

import com.feyconsuelo.application.service.googledrive.GoogleDriveService;
import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.googledrive.FileResponse;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UpdatePerformanceImpl {

    private final PerformanceService performanceService;

    private final ResizeImageImpl resizeImageService;

    private final GoogleDriveService googleDriveService;

    @Value("${default-images.event}")
    private String defaultEventImage;

    @Value("${config-google-drive.folder-events-google-id}")
    private String folderEventsGoogleId;

    public void update(final Long eventId, final EventRequest eventRequest) {

        final Optional<EventResponse> eventResponseThumbnailImage = this.performanceService.getById(eventId, true, false);
        final Optional<EventResponse> eventResponseOriginalImage = this.performanceService.getById(eventId, false, false);

        if (eventResponseThumbnailImage.isEmpty() || eventResponseOriginalImage.isEmpty()) {
            throw new NotFoundException("No existe la actuacion que desea actualizar");
        }

        // si la imagen que viene es igual que e thumbnail, no la guardamos
        // eventRequest.getImage(), trae el thumbnail (pq es el que devolvimos en el listado)
        // eventResponseThumbnailImage.getImage(), tiene la imagen thumbnail (pq hemnos pasado true)
        if (eventRequest.getImage() != null && eventRequest.getImage().equals(eventResponseThumbnailImage.get().getImage())) {
            eventRequest.setImage(eventResponseOriginalImage.get().getImage());
        }

        // si estan enviando imagen, debemos redimensionarla
        if (StringUtils.isNotEmpty(eventRequest.getImage()) && !eventRequest.getImage().equals(this.defaultEventImage)) {
            eventRequest.setImageThumbnail(this.resizeImageService.resizeImage(eventRequest.getImage()));
        }

        // si estamos modificando, entonces tendremos que modificar tambien el nombre del directorio de google drive, por si ha cambiado
        if (Boolean.FALSE.equals(StringUtils.isEmpty(eventResponseThumbnailImage.get().getGoogleId()))) {
            this.googleDriveService.renameFolder(eventResponseThumbnailImage.get().getGoogleId(), eventRequest.getTitle());
            eventRequest.setGoogleId(eventResponseThumbnailImage.get().getGoogleId());
        } else {
            final Optional<FileResponse> folder = this.googleDriveService.createFolder(eventRequest.getTitle(), this.folderEventsGoogleId);
            folder.ifPresent(fileResponse -> eventRequest.setGoogleId(fileResponse.getGoogleId()));
        }

        this.performanceService.update(eventResponseThumbnailImage.get().getId(), eventRequest);

    }

}
