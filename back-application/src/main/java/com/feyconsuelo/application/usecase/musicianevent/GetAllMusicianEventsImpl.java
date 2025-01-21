package com.feyconsuelo.application.usecase.musicianevent;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.usecase.performance.GetAllPerformanceImpl;
import com.feyconsuelo.application.usecase.rehearsal.GetAllRehearsalImpl;
import com.feyconsuelo.application.usecase.statistics.musicianevent.StatisticsMusicianAssistEventsImpl;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventListResponse;
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
    private final MusicianService musicianService;
    private final StatisticsMusicianAssistEventsImpl statisticsAssistEvents;

    private LocalDate getStartDate(final LocalDate startDate, final MusicianResponse musician) {
        if (startDate == null) {
            return musician.getRegistrationDate().toLocalDate();
        } else {
            if (musician.getRegistrationDate().toLocalDate().isAfter(startDate)) {
                return musician.getRegistrationDate().toLocalDate();
            } else {
                return startDate;
            }
        }
    }

    @Override
    public MusicianEventListResponse execute(final Long musicianId, final LocalDate startDate, final LocalDate endDate, final EventTypeEnum eventType) {

        // obtenemos el musico
        final Optional<MusicianResponse> musician = this.musicianService.get(musicianId, Boolean.TRUE);

        if (musician.isEmpty()) {
            throw new BadRequestException("No existe el musico");
        }

        List<EventResponse> rehearsalList = new ArrayList<>();
        List<EventResponse> performanceList = new ArrayList<>();
        if (eventType == null || EventTypeEnum.REHEARSAL.equals(eventType)) {
            rehearsalList = new ArrayList<>(this.getAllRehearsal.execute(this.getStartDate(startDate, musician.get()), endDate, Optional.ofNullable(musicianId)));
        }

        if (eventType == null || EventTypeEnum.PERFORMANCE.equals(eventType)) {
            performanceList = new ArrayList<>(this.getAllPerformance.execute(this.getStartDate(startDate, musician.get()), endDate, Optional.ofNullable(musicianId)));
        }

        final List<EventResponse> events = Stream.concat(
                        rehearsalList.stream()
                                .filter(rehearsal -> rehearsal.getVoiceIdList().contains(musician.get().getVoice().getId().intValue())),
                        performanceList.stream()
                                .filter(performance -> performance.getVoiceIdList().contains(musician.get().getVoice().getId().intValue()))
                )
                .sorted(Comparator.comparing(EventResponse::getDate))
                .toList();

        return MusicianEventListResponse.builder()
                .eventAssistStatisticsResponse(null)
                .musicianEventAssistStatistics(this.statisticsAssistEvents.getPercentageAssistEvents(
                                musicianId,
                                this.getStartDate(startDate, musician.get()),
                                endDate
                        )
                )
                .musicianAssitsInformation(null)
                .events(events)
                .build();

    }
}
