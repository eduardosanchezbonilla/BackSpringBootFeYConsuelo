package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.user.TokenInfoExtractorService;
import com.feyconsuelo.application.usecase.performance.GetAllPerformanceImpl;
import com.feyconsuelo.application.usecase.rehearsal.GetAllRehearsalImpl;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.user.UserRoleEnum;
import com.feyconsuelo.domain.usecase.event.GetAllEvents;
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
public class GetAllEventsImpl implements GetAllEvents {

    private final GetAllPerformanceImpl getAllPerformance;
    private final GetAllRehearsalImpl getAllRehearsal;
    private final TokenInfoExtractorService tokenInfoExtractorService;
    private final MusicianService musicianService;

    private Optional<Long> getMusicianId() {
        if (Boolean.TRUE.equals(this.tokenInfoExtractorService.hasRole(UserRoleEnum.MUSICO.getId()))) {
            final Optional<MusicianResponse> musician = this.musicianService.getByDni(this.tokenInfoExtractorService.getUsername().toUpperCase());

            if (musician.isPresent()) {
                return Optional.of(musician.get().getId());
            }
        }
        return Optional.empty();
    }

    @Override
    public List<EventResponse> execute(final LocalDate startDate, final LocalDate endDate, final EventTypeEnum eventType) {
        List<EventResponse> rehearsalList = new ArrayList<>();
        List<EventResponse> performanceList = new ArrayList<>();
        final Optional<Long> musicianId = this.getMusicianId();
        if (eventType == null || EventTypeEnum.REHEARSAL.equals(eventType)) {
            rehearsalList = new ArrayList<>(this.getAllRehearsal.execute(startDate, endDate, musicianId));
        }

        if (eventType == null || EventTypeEnum.PERFORMANCE.equals(eventType)) {
            performanceList = new ArrayList<>(this.getAllPerformance.execute(startDate, endDate, musicianId));
        }

        return Stream.concat(rehearsalList.stream(), performanceList.stream()) // Unir ambas listas
                .sorted(Comparator.comparing(EventResponse::getDate))     // Ordenar por startDate
                .toList();
    }
}
