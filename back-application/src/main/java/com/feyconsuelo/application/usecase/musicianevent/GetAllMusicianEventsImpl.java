package com.feyconsuelo.application.usecase.musicianevent;

import com.feyconsuelo.application.usecase.performance.GetAllPerformanceImpl;
import com.feyconsuelo.application.usecase.rehearsal.GetAllRehearsalImpl;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.musicianevent.GetAllMusicianEvents;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

@Component
@RequiredArgsConstructor
public class GetAllMusicianEventsImpl implements GetAllMusicianEvents {

    private final GetAllPerformanceImpl getAllPerformance;
    private final GetAllRehearsalImpl getAllRehearsal;

    @Override
    public List<EventResponse> execute(final Long musicianId, final LocalDate startDate, final LocalDate endDate, final EventTypeEnum eventType) {
        List<EventResponse> rehearsalList = new ArrayList<>();
        List<EventResponse> performanceList = new ArrayList<>();
        if (eventType == null || EventTypeEnum.REHEARSAL.equals(eventType)) {
            rehearsalList = new ArrayList<>(this.getAllRehearsal.execute(startDate, endDate, Optional.ofNullable(musicianId)));
        }

        if (eventType == null || EventTypeEnum.PERFORMANCE.equals(eventType)) {
            performanceList = new ArrayList<>(this.getAllPerformance.execute(startDate, endDate, Optional.ofNullable(musicianId)))
            ;
        }

        return Stream.concat(rehearsalList.stream(), performanceList.stream()) // Unir ambas listas
                .sorted(Comparator.comparing(EventResponse::getDate))     // Ordenar por startDate
                .toList();
    }
}
