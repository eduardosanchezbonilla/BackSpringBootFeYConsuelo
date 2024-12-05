package com.feyconsuelo.application.usecase.performance;

import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.application.service.rehearsal.RehearsalService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventResponse;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class InsertPerformanceImpl {

    private final RehearsalService rehearsalService;

    private final PerformanceService performanceService;

    private final ResizeImageImpl resizeImageService;

    @Value("${default-images.event}")
    private String defaultEventImage;

    public void insertPerformance(final EventRequest eventRequest) {

        // si ya existe un ensayo ese dia, no dejamos meter la actuacion
        final Optional<EventResponse> rehearsal = this.rehearsalService.getByDate(eventRequest.getDate());

        if (rehearsal.isPresent()) {
            throw new BadRequestException("No se puede introducir una actuacion el mismo dia de un ensayo");
        }

        // no comprobamos que exista actuacion mismo dia, porque permitiremos que haya varias actuaciones el mismo dia
        
        // si estan enviando imagen y no es la imagen por defecto, debemos redimensionarla
        if (StringUtils.isNotEmpty(eventRequest.getImage()) && !eventRequest.getImage().equals(this.defaultEventImage)) {
            eventRequest.setImage(this.resizeImageService.resizeImage(eventRequest.getImage()));
        }

        // insertamos
        this.performanceService.insert(eventRequest);

    }

}
