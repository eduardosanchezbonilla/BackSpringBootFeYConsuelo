package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.musicianrehearsal.MusicianRehearsalService;
import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.application.service.rehearsal.RehearsalService;
import com.feyconsuelo.domain.model.event.EventMusicianAssistanceResponse;
import com.feyconsuelo.domain.model.event.EventMusiciansResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.usecase.event.GetEventMusicianAssistance;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetEventMusicianAssistanceImpl implements GetEventMusicianAssistance {

    private final MusicianRehearsalService musicianRehearsalService;
    private final RehearsalService rehearsalService;
    private final PerformanceService performanceService;
    private final MusicianService musicianService;

    @Override
    public Optional<EventMusicianAssistanceResponse> execute(final EventTypeEnum eventType, final Long eventId, final Boolean returnFakeMusicians) {

        // obtengo el ultimo ensayo realizado hasta este momento
        if (EventTypeEnum.REHEARSAL.equals(eventType)) {

            final Optional<EventMusiciansResponse> eventMusiciansResponse = this.rehearsalService.getEventMusicians(eventId);
            if (eventMusiciansResponse.isPresent()) {

                final List<MusicianResponse> musicians = Boolean.TRUE.equals(returnFakeMusicians) ?
                        eventMusiciansResponse.get().getMusicians() :
                        eventMusiciansResponse.get().getMusicians().stream()
                                .filter(musician -> musician.getId() >= 0)
                                .toList();

                return Optional.of(
                        EventMusicianAssistanceResponse.builder()
                                .event(eventMusiciansResponse.get().getEvent())
                                .musicians(musicians)
                                .build()
                );
            }
        } else {
            final Optional<EventMusiciansResponse> eventMusiciansResponse = this.performanceService.getEventMusicians(eventId, true, false);
            if (eventMusiciansResponse.isPresent()) {

                final List<MusicianResponse> musicians = Boolean.TRUE.equals(returnFakeMusicians) ?
                        eventMusiciansResponse.get().getMusicians() :
                        eventMusiciansResponse.get().getMusicians().stream()
                                .filter(musician -> musician.getId() >= 0)
                                .toList();

                return Optional.of(
                        EventMusicianAssistanceResponse.builder()
                                .event(eventMusiciansResponse.get().getEvent())
                                .musicians(musicians)
                                .build()
                );
            }
        }
        return Optional.empty();
    }
}
