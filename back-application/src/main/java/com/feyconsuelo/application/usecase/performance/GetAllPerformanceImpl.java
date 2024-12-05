package com.feyconsuelo.application.usecase.performance;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.musicianperformance.MusicianPerformanceService;
import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.application.service.user.TokenInfoExtractorService;
import com.feyconsuelo.domain.model.event.EventClsClassEnum;
import com.feyconsuelo.domain.model.event.EventResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
public class GetAllPerformanceImpl {

    private final PerformanceService performanceService;

    private final MusicianPerformanceService musicianPerformanceService;

    private final MusicianService musicianService;

    private final TokenInfoExtractorService tokenInfoExtractorService;

    private List<EventResponse> getMusicianPerformance(final LocalDate startDate, final LocalDate endDate, final Optional<Long> musicianId) {
        List<EventResponse> musicianPerformance = new ArrayList<>();
        if (musicianId.isPresent()) {
            musicianPerformance = this.musicianPerformanceService.getAll(musicianId.get(), startDate, endDate);
        }
        return musicianPerformance;
    }

    public void updateFieldForMultipleEvents(final List<EventResponse> performance) {
        // Agrupar eventos por fecha
        final Map<LocalDate, List<EventResponse>> eventsByDate = performance.stream()
                .collect(Collectors.groupingBy(EventResponse::getDate));

        // Procesar solo las fechas con múltiples eventos
        eventsByDate.forEach((date, events) -> {
            // Verificar si hay más de un evento y si los valores de clsClass son distintos
            final boolean hasDistinctClsClass = events.stream()
                    .map(EventResponse::getClsClass)
                    .distinct()
                    .count() > 1;

            if (events.size() > 1 && hasDistinctClsClass) {
                // Actualizar el campo para eventos en esta fecha
                events.forEach(event -> event.setClsClass(EventClsClassEnum.ACTUACION_DAY_OK_KO));
            }
        });
    }

    public List<EventResponse> execute(final LocalDate startDate, final LocalDate endDate, final Optional<Long> musicianId) {
        final List<EventResponse> musicianPerformance = this.getMusicianPerformance(startDate, endDate, musicianId);
        final List<EventResponse> performance = this.performanceService.getAll(startDate, endDate);

        if (CollectionUtils.isEmpty(musicianPerformance)) {
            return performance;
        }

        // para cada rehearsal, miro si existe en musicianRehearsal. Si existe, le cambio del clsClass
        for (final EventResponse eventResponse : performance) {
            for (final EventResponse musicianEventResponse : musicianPerformance) {
                if (eventResponse.getId().equals(musicianEventResponse.getId())) {
                    eventResponse.setClsClass(musicianEventResponse.getClsClass());
                    eventResponse.setBus(musicianEventResponse.getBus());
                    eventResponse.setAssist(Boolean.TRUE);
                }
            }
        }

        // por ultimo, si tengo en el array el mismo dia varias veces, y cambia el campo clsClass, tengo que poner a todos esos elementos uno especifico
        this.updateFieldForMultipleEvents(performance);

        return performance;
    }
}
