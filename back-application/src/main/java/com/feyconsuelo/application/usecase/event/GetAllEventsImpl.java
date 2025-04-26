package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.user.TokenInfoExtractorService;
import com.feyconsuelo.application.usecase.performance.GetAllPerformanceImpl;
import com.feyconsuelo.application.usecase.rehearsal.GetAllRehearsalImpl;
import com.feyconsuelo.application.usecase.statistics.event.StatisticsAssistEventsImpl;
import com.feyconsuelo.application.usecase.statistics.musicianevent.StatisticsMusicianAssistEventsImpl;
import com.feyconsuelo.application.usecase.statistics.repertoiremarch.StatisticsRepertoireMarchEventsImpl;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventListResponse;
import com.feyconsuelo.domain.model.statistics.EventAssistStatisticsResponse;
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
    private final StatisticsMusicianAssistEventsImpl statisticsMusicianAssistEvents;
    private final StatisticsAssistEventsImpl statisticsAssistEvents;
    private final StatisticsRepertoireMarchEventsImpl statisticsRepertoireMarchEvents;

    private Optional<MusicianResponse> getMusicianId() {
        if (Boolean.TRUE.equals(this.tokenInfoExtractorService.hasRole(UserRoleEnum.MUSICO.getId()))) {
            final Optional<MusicianResponse> musician = this.musicianService.getByDni(this.tokenInfoExtractorService.getUsername().toUpperCase(), Boolean.TRUE);

            if (musician.isPresent()) {
                return musician;
            }
        }
        return Optional.empty();
    }

    private LocalDate getStartDate(final LocalDate startDate) {
        return startDate;
    }

    private LocalDate getEndDate(final LocalDate endDate) {
        return endDate;
    }

    @Override
    @SuppressWarnings("java:S3776")
    public MusicianEventListResponse execute(final LocalDate startDate, final LocalDate endDate, final EventTypeEnum eventType, final Boolean isTodayPerformance) {
        List<EventResponse> rehearsalList = new ArrayList<>();
        List<EventResponse> performanceList = new ArrayList<>();
        final Optional<MusicianResponse> musician = Boolean.FALSE.equals(isTodayPerformance) ? this.getMusicianId() : Optional.empty();
        if (eventType == null || EventTypeEnum.REHEARSAL.equals(eventType)) {
            rehearsalList = new ArrayList<>(this.getAllRehearsal.execute(this.getStartDate(startDate), this.getEndDate(endDate), musician.map(MusicianResponse::getId)));
        }

        if (eventType == null || EventTypeEnum.PERFORMANCE.equals(eventType)) {
            performanceList = new ArrayList<>(this.getAllPerformance.execute(this.getStartDate(startDate), this.getEndDate(endDate), musician.map(MusicianResponse::getId), null));

            if (Boolean.FALSE.equals(this.tokenInfoExtractorService.hasRole(UserRoleEnum.SUPER_ADMIN.getId()))) {
                performanceList = performanceList.stream().filter(event -> Boolean.TRUE.equals(event.getEventPublic())).toList();
            }
        }

        final List<EventResponse> events = Stream.concat(rehearsalList.stream(), performanceList.stream()) // Unir ambas listas
                .sorted(Comparator.comparing(EventResponse::getDate))     // Ordenar por startDate
                .toList();

        if (Boolean.TRUE.equals(this.tokenInfoExtractorService.hasRole(UserRoleEnum.INVITADO.getId()))) {
            return MusicianEventListResponse.builder()
                    .eventAssistStatisticsResponse(null)
                    .musicianEventAssistStatistics(null)
                    .musicianAssitsInformation(null)
                    .repertoireMarchEventStatistic(null)
                    .events(events)
                    .build();
        } else {
            // obtenemos estadisticas generales de asistencia
            final EventAssistStatisticsResponse assistStatistics = Boolean.FALSE.equals(isTodayPerformance) ?
                    this.statisticsAssistEvents.getStatisticsAssistEvents(startDate, endDate) :
                    EventAssistStatisticsResponse.builder().build();

            // si hay musico, entonces filtramos por su fecha de incorporacion y por idvoz
            if (musician.isPresent() && eventType == null) { // pongo la condicion del == a null, para que en la vista de actuaciones, no calculemos estadisticas
                final List<EventResponse> filterEvents = events.stream()
                        .filter(event -> event.getVoiceIdList().contains(musician.get().getVoice().getId().intValue()) || musician.get().getVoice().getName().contains("ANTIGUO"))
                        .sorted(Comparator.comparing(EventResponse::getDate))
                        .toList();

                return MusicianEventListResponse.builder()
                        .eventAssistStatisticsResponse(assistStatistics)
                        .musicianEventAssistStatistics(this.statisticsMusicianAssistEvents.getPercentageAssistEvents(
                                        musician.get().getId(),
                                        this.getStartDate(startDate),
                                        endDate
                                )
                        )
                        .musicianAssitsInformation(null)
                        .repertoireMarchEventStatistic(null)
                        .events(filterEvents)
                        .build();
            } else {
                return MusicianEventListResponse.builder()
                        .eventAssistStatisticsResponse(assistStatistics)
                        .musicianEventAssistStatistics(null)
                        .musicianAssitsInformation(Boolean.FALSE.equals(isTodayPerformance) ? this.statisticsMusicianAssistEvents.getMusicianAssistInformation(startDate, endDate) : null)
                        .repertoireMarchEventStatistic(Boolean.FALSE.equals(isTodayPerformance) ? this.statisticsRepertoireMarchEvents.getRepertoireMarchEventStatistic(startDate, endDate) : null)
                        .events(events)
                        .build();
            }
        }
    }

}
