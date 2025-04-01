package com.feyconsuelo.application.usecase.performance;

import com.feyconsuelo.application.service.googledrive.GoogleDriveService;
import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.event.EventResponse;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class DeletePerformanceImpl {

    private final PerformanceService performanceService;
    private final GoogleDriveService googleDriveService;

    public void execute(final Long eventId) {
        final Optional<EventResponse> eventResponse = this.performanceService.getById(eventId, true, false);

        if (eventResponse.isEmpty()) {
            throw new NotFoundException("No existe la actuacion que desea eliminar");
        } else {
            // si tenemos google id, eliminamos el directorio de google drive
            if (Boolean.FALSE.equals(StringUtils.isEmpty(eventResponse.get().getGoogleId()))) {
                googleDriveService.deleteFolderRecursively(eventResponse.get().getGoogleId());
            }

            this.performanceService.logicalDelete(eventId);
        }
    }

}
