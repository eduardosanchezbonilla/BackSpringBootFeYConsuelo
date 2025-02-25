package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.musicianperformance.MusicianPerformanceService;
import com.feyconsuelo.application.service.musicianrehearsal.MusicianRehearsalService;
import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.application.service.rehearsal.RehearsalService;
import com.feyconsuelo.domain.model.event.EventMusicianAssistanceResponse;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventResponse;
import com.feyconsuelo.domain.usecase.event.GetEventMusicianAssistance;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetEventMusicianAssistanceImpl implements GetEventMusicianAssistance {

    private final MusicianRehearsalService musicianRehearsalService;
    private final MusicianPerformanceService musicianPerformanceService;
    private final RehearsalService rehearsalService;
    private final PerformanceService performanceService;
    private final MusicianService musicianService;

    @Override
    public Optional<EventMusicianAssistanceResponse> execute(final EventTypeEnum eventType, final Long eventId) {

        // obtengo el ultimo ensayo realizado hasta este momento
        if (EventTypeEnum.REHEARSAL.equals(eventType)) {
            final Optional<EventResponse> eventResponse = this.rehearsalService.getById(eventId);

            if (eventResponse.isPresent()) {
                // obtenemos todos los musicos
                final List<MusicianResponse> musicians = this.musicianService.getAll(Boolean.FALSE);
                final List<MusicianEventResponse> musicianEventResponseList = this.musicianRehearsalService.findAllActivesMusiciansByRehearsalId(eventResponse.get().getId());

                // asigno a cada musico su asistencia al ultimo ensayo
                musicians.forEach(
                        musician -> {
                            musician.setAssistLastRehearsal(
                                    musicianEventResponseList.stream()
                                            .anyMatch(musicianEventResponse -> musicianEventResponse.getMusicianResponse().getId().equals(musician.getId()))
                            );
                            musician.setFormationPositionX(
                                    musicianEventResponseList.stream()
                                            .filter(musicianEventResponse -> musicianEventResponse.getMusicianResponse().getId().equals(musician.getId()))
                                            .map(musicianEventResponse -> musicianEventResponse.getEventResponse().getFormationPositionX())
                                            .filter(Objects::nonNull)
                                            .findFirst()
                                            .orElse(null)
                            );
                            musician.setFormationPositionY(
                                    musicianEventResponseList.stream()
                                            .filter(musicianEventResponse -> musicianEventResponse.getMusicianResponse().getId().equals(musician.getId()))
                                            .map(musicianEventResponse -> musicianEventResponse.getEventResponse().getFormationPositionY())
                                            .filter(Objects::nonNull)
                                            .findFirst()
                                            .orElse(null)
                            );
                        }
                );

                return Optional.of(EventMusicianAssistanceResponse.builder()
                        .event(eventResponse.get())
                        .musicians(musicians)
                        .build());
            }
        } else {
            final Optional<EventResponse> eventResponse = this.performanceService.getById(eventId, true, false);
            if (eventResponse.isPresent()) {
                final List<MusicianResponse> musicians = this.musicianService.getAll(Boolean.FALSE);
                final List<MusicianEventResponse> musicianEventResponseList = this.musicianPerformanceService.findAllActivesMusiciansByPerformanceId(eventResponse.get().getId());

                // asigno a cada musico su asistencia al ultimo ensayo
                musicians.forEach(
                        musician -> {
                            musician.setAssistLastRehearsal(
                                    musicianEventResponseList.stream()
                                            .anyMatch(musicianEventResponse -> musicianEventResponse.getMusicianResponse().getId().equals(musician.getId()))
                            );
                            musician.setAssistBus(
                                    musicianEventResponseList.stream()
                                            .anyMatch(
                                                    musicianEventResponse ->
                                                            musicianEventResponse.getMusicianResponse().getId().equals(musician.getId()) &&
                                                                    musicianEventResponse.getEventResponse().getMusicianBus() != null &&
                                                                    musicianEventResponse.getEventResponse().getMusicianBus()
                                            )
                            );
                            musician.setFormationPositionX(
                                    musicianEventResponseList.stream()
                                            .filter(musicianEventResponse -> musicianEventResponse.getMusicianResponse().getId().equals(musician.getId()))
                                            .map(musicianEventResponse -> musicianEventResponse.getEventResponse().getFormationPositionX())
                                            .filter(Objects::nonNull)
                                            .findFirst()
                                            .orElse(null)
                            );
                            musician.setFormationPositionY(
                                    musicianEventResponseList.stream()
                                            .filter(musicianEventResponse -> musicianEventResponse.getMusicianResponse().getId().equals(musician.getId()))
                                            .map(musicianEventResponse -> musicianEventResponse.getEventResponse().getFormationPositionY())
                                            .filter(Objects::nonNull)
                                            .findFirst()
                                            .orElse(null)
                            );
                        }
                );

                return Optional.of(EventMusicianAssistanceResponse.builder()
                        .event(eventResponse.get())
                        .musicians(musicians)
                        .build());
            }
        }
        return Optional.empty();
    }
}
