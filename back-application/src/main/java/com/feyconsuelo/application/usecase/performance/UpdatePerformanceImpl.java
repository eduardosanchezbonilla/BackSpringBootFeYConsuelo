package com.feyconsuelo.application.usecase.performance;

import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventResponse;
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

    @Value("${default-images.event}")
    private String defaultEventImage;

    public void update(final Long eventId, final EventRequest eventRequest) {

        final Optional<EventResponse> eventResponse = this.performanceService.getById(eventId);

        if (eventResponse.isEmpty()) {
            throw new NotFoundException("No existe la actuacion que desea actualizar");
        }

        // si estan enviando imagen, debemos redimensionarla
        if (StringUtils.isNotEmpty(eventRequest.getImage()) && !eventRequest.getImage().equals(this.defaultEventImage)) {
            eventRequest.setImage(this.resizeImageService.resizeImage(eventRequest.getImage()));
        }

        this.performanceService.update(eventResponse.get().getId(), eventRequest);

    }

}
