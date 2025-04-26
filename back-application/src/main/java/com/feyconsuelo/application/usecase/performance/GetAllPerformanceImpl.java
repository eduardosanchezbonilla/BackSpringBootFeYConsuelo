package com.feyconsuelo.application.usecase.performance;

import com.feyconsuelo.application.service.musicianperformance.MusicianPerformanceService;
import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.application.service.user.TokenInfoExtractorService;
import com.feyconsuelo.domain.model.event.EventClsClassEnum;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.user.UserRoleEnum;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
public class GetAllPerformanceImpl {

    private final PerformanceService performanceService;

    private final MusicianPerformanceService musicianPerformanceService;

    private final TokenInfoExtractorService tokenInfoExtractorService;

    public List<EventResponse> execute(final LocalDate startDate, final LocalDate endDate, final Optional<Long> musicianId, final Boolean isSuperAdmin) {
        if (musicianId.isEmpty()) {
            List<EventResponse> performance = this.performanceService.getAll(startDate, endDate);

            // aqui dependiendo del role, tenemos que mirar si el evento esta publicado o no para devolverlo
            if (Boolean.FALSE.equals(this.tokenInfoExtractorService.hasRole(UserRoleEnum.SUPER_ADMIN.getId())) && (Boolean.FALSE.equals(isSuperAdmin))) {
                performance = performance.stream().filter(event -> Boolean.TRUE.equals(event.getEventPublic())).toList();
            }

            return performance;
        } else {
            List<EventResponse> musicianPerformance = this.musicianPerformanceService.getAllMusicianPerformance(
                    musicianId.get(),
                    startDate,
                    endDate
            );

            // aqui dependiendo del role, tenemos que mirar si el evento esta publicado o no para devolverlo
            if (Boolean.FALSE.equals(this.tokenInfoExtractorService.hasRole(UserRoleEnum.SUPER_ADMIN.getId())) && (Boolean.FALSE.equals(isSuperAdmin))) {
                musicianPerformance = musicianPerformance.stream().filter(event -> Boolean.TRUE.equals(event.getEventPublic())).toList();
            }

            // por ultimo, si tengo en el array el mismo dia varias veces, y cambia el campo clsClass, tengo que poner a todos esos elementos uno especifico
            this.updateFieldForMultipleEvents(musicianPerformance);

            return musicianPerformance;
        }

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

}
