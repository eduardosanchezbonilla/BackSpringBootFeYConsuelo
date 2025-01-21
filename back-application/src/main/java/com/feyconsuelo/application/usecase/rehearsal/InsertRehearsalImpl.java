package com.feyconsuelo.application.usecase.rehearsal;

import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.application.service.rehearsal.RehearsalService;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

@Component
@RequiredArgsConstructor
public class InsertRehearsalImpl {

    private final RehearsalService rehearsalService;

    private final PerformanceService performanceService;

    private List<LocalDate> getDates(final EventRequest rehearsalRequest) {
        final List<LocalDate> dates;

        if (rehearsalRequest.getRepetitionPeriod() != null && rehearsalRequest.getEndDate() != null) {
            if (rehearsalRequest.getDate().isAfter(rehearsalRequest.getEndDate())) {
                throw new IllegalArgumentException("La fecha de inicio debe ser anterior o igual a la fecha de fin");
            }

            if (rehearsalRequest.getEndDate().isAfter(rehearsalRequest.getDate().plusYears(1))) {
                throw new BadRequestException("La fecha de fin de repeticion no puede ser mayor a 1 año de la fecha de inicio");
            }

            dates = Stream.iterate(
                    rehearsalRequest.getDate(),
                    date -> !date.isAfter(rehearsalRequest.getEndDate()),
                    date -> date.plus(rehearsalRequest.getRepetitionPeriod().getPeriod())
            ).toList();
        } else {
            dates = List.of(rehearsalRequest.getDate());
        }
        return dates;
    }

    public void insertRehearsal(final EventRequest rehearsalRequest) {

        // recorremos las fechas
        this.getDates(rehearsalRequest).forEach(
                date -> {
                    // solo tenemos en cuenta la fecha, si no hay actuacion ese dia
                    final Optional<EventResponse> performance = this.performanceService.getByDate(date);

                    if (performance.isEmpty()) {

                        final Optional<EventResponse> rehearsal = this.rehearsalService.getByDate(date);

                        // si no existe ensayo ese dia, insertamos, sino modificamos
                        rehearsalRequest.setDate(date);
                        rehearsalRequest.setStartTime(rehearsalRequest.getStartTime().with(date));
                        rehearsalRequest.setEndTime(rehearsalRequest.getEndTime().with(date));

                        if (rehearsal.isEmpty()) {
                            this.rehearsalService.insert(rehearsalRequest);
                        } else {
                            this.rehearsalService.update(rehearsal.get().getId(), rehearsalRequest);
                        }
                    }
                }
        );
    }

}
