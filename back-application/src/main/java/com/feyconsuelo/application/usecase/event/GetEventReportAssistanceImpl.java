package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.application.service.rehearsal.RehearsalService;
import com.feyconsuelo.application.service.report.EventReportService;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.model.event.EventMusicianAssistanceResponse;
import com.feyconsuelo.domain.model.event.EventReportAssistanceResponse;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.usecase.event.GetEventReportAssistance;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetEventReportAssistanceImpl implements GetEventReportAssistance {

    private final RehearsalService rehearsalService;
    private final PerformanceService performanceService;
    private final GetEventMusicianAssistanceImpl getEventMusicianAssistance;
    private final EventReportService eventReportService;
    @Value("${default-images.header-report}")
    private String defaultHeaderReport;
    @Value("${default-images.badge}")
    private String defaultBadge;

    @Override
    public Optional<EventReportAssistanceResponse> execute(final EventTypeEnum eventType, final Long eventId) {

        final Optional<EventResponse> eventResponse;

        if (EventTypeEnum.REHEARSAL.equals(eventType)) {
            eventResponse = this.rehearsalService.getById(eventId);
        } else {
            eventResponse = this.performanceService.getById(eventId, true, false);
        }

        if (eventResponse.isEmpty()) {
            throw new BadRequestException("No existe el evento");
        }

        // obtenemos la asistencia
        final Optional<EventMusicianAssistanceResponse> eventMusicianAssistanceResponse = this.getEventMusicianAssistance.execute(eventType, eventId);

        if (eventMusicianAssistanceResponse.isEmpty()) {
            throw new BadRequestException("No se ha podido obtener la asistencia de los músicos");
        }

        try {
            final String report = this.eventReportService.getEventReportAssistance(
                    EventTypeEnum.PERFORMANCE.equals(eventResponse.get().getType()) ?
                            eventResponse.get().getTitle() :
                            "Ensayo General (" + eventResponse.get().getDate() + ")",
                    eventResponse.get().getDisplacementBus(),
                    Boolean.TRUE.equals(eventResponse.get().getDisplacementBus()) ?
                            eventMusicianAssistanceResponse.get().getMusicians().stream()
                                    .filter(musician -> Boolean.TRUE.equals(musician.getAssistLastRehearsal()) &&
                                            Boolean.TRUE.equals(musician.getAssistBus()) &&
                                            eventResponse.get().getVoiceIdList().contains(musician.getVoice().getId().intValue())
                                    )
                                    .sorted(
                                            Comparator.comparing(MusicianResponse::getVoiceOrder)
                                                    .thenComparing(MusicianResponse::getSurname)
                                    )
                                    .toList() :
                            List.of(),
                    Boolean.TRUE.equals(eventResponse.get().getDisplacementBus()) ?
                            eventMusicianAssistanceResponse.get().getMusicians().stream()
                                    .filter(musician -> Boolean.TRUE.equals(musician.getAssistLastRehearsal()) &&
                                            Boolean.FALSE.equals(musician.getAssistBus()) &&
                                            eventResponse.get().getVoiceIdList().contains(musician.getVoice().getId().intValue())
                                    )
                                    .sorted(
                                            Comparator.comparing(MusicianResponse::getVoiceOrder)
                                                    .thenComparing(MusicianResponse::getSurname)
                                    )
                                    .toList() :
                            eventMusicianAssistanceResponse.get().getMusicians().stream()
                                    .filter(musician -> Boolean.TRUE.equals(musician.getAssistLastRehearsal()) &&
                                            eventResponse.get().getVoiceIdList().contains(musician.getVoice().getId().intValue())
                                    )
                                    .sorted(
                                            Comparator.comparing(MusicianResponse::getVoiceOrder)
                                                    .thenComparing(MusicianResponse::getSurname)
                                    )
                                    .toList(),
                    eventMusicianAssistanceResponse.get().getMusicians().stream()
                            .filter(musician -> Boolean.FALSE.equals(musician.getAssistLastRehearsal()) && eventResponse.get().getVoiceIdList().contains(musician.getVoice().getId().intValue()))
                            .sorted(
                                    Comparator.comparing(MusicianResponse::getVoiceOrder)
                                            .thenComparing(MusicianResponse::getSurname)
                            )
                            .toList(),
                    this.defaultBadge,
                    this.defaultHeaderReport
            );
            return Optional.of(
                    EventReportAssistanceResponse.builder()
                            .event(eventResponse.get())
                            .report(report)
                            .build()
            );
        } catch (final IOException e) {
            throw new FeYConsueloException(e);
        }
    }
}
