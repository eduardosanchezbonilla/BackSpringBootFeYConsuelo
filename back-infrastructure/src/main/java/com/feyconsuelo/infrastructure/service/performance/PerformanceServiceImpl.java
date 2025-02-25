package com.feyconsuelo.infrastructure.service.performance;

import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.event.EventFormationRequest;
import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventRouteRequest;
import com.feyconsuelo.domain.model.event.LatLng;
import com.feyconsuelo.domain.model.musician.MusicianFormationRequest;
import com.feyconsuelo.infrastructure.converter.performance.EventRequestToPerformanceEntityConverter;
import com.feyconsuelo.infrastructure.converter.performance.PerformanceEntityListToEventResponseListConverter;
import com.feyconsuelo.infrastructure.converter.performance.PerformanceEntityToEventResponseConverter;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceEntity;
import com.feyconsuelo.infrastructure.entities.performance.PerformanceEntity;
import com.feyconsuelo.infrastructure.repository.MusicianPerformanceRepository;
import com.feyconsuelo.infrastructure.repository.PerformanceRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class PerformanceServiceImpl implements PerformanceService {
    private final PerformanceRepository performanceRepository;
    private final EventRequestToPerformanceEntityConverter eventRequestToPerformanceEntityConverter;
    private final PerformanceEntityListToEventResponseListConverter performanceEntityListToEventResponseListConverter;
    private final PerformanceEntityToEventResponseConverter performanceEntityToEventResponseConverter;
    private final MusicianPerformanceRepository musicianPerformanceRepository;

    @Override
    public List<EventResponse> getAll(final LocalDate startDate, final LocalDate endDate) {
        final List<PerformanceEntity> performanceList = this.performanceRepository.findAllActives(
                startDate,
                endDate,
                startDate == null,
                endDate == null
        );
        return this.performanceEntityListToEventResponseListConverter.convert(performanceList);
    }

    @Override
    public Optional<EventResponse> getById(final Long eventId, final Boolean isThumbnail, final Boolean route) {
        final var event = this.performanceRepository.findPerformanceActiveById(eventId);
        return event.map(ev -> this.performanceEntityToEventResponseConverter.convert(ev, isThumbnail, route));
    }

    @Override
    public Optional<EventResponse> getByDate(final LocalDate date) {
        final var event = this.performanceRepository.findPerformanceActiveByDate(date);
        return event.map(ev -> this.performanceEntityToEventResponseConverter.convert(ev, true, false));
    }

    @Override
    public void insert(final EventRequest eventRequest) {
        this.performanceRepository.save(
                this.eventRequestToPerformanceEntityConverter.convert(eventRequest)
        );
    }

    @Override
    public void update(final Long eventId, final EventRequest eventRequest) {
        final var performance = this.performanceRepository.findPerformanceActiveById(eventId);

        if (performance.isEmpty()) {
            throw new NotFoundException("No existe la actuación que desea modificar");
        }

        this.performanceRepository.save(
                this.eventRequestToPerformanceEntityConverter.updateEntity(performance.get(), eventRequest)
        );
    }

    @Override
    public void delete(final Long eventId) {
        this.performanceRepository.deleteById(eventId);
    }

    @Override
    public void logicalDelete(final Long eventId) {
        final var event = this.performanceRepository.findPerformanceActiveById(eventId);

        if (event.isEmpty()) {
            throw new NotFoundException("No existe la actuacion que desea eliminar");
        }

        this.performanceRepository.save(this.eventRequestToPerformanceEntityConverter.deleteEntity(event.get()));
    }

    @Override
    public void updateFormation(final Long eventId, final EventFormationRequest eventFormationRequest) {
        // despues uno a uno vamos actualizando su posicion
        final List<MusicianPerformanceEntity> musicians = this.musicianPerformanceRepository.findAllActivesMusiciansByPerformanceId(eventId);

        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(musicians))) {
            musicians.forEach(musician -> {
                final Optional<MusicianFormationRequest> position = eventFormationRequest.getMusicians().stream()
                        .filter(musicianFormationRequest -> musicianFormationRequest.getMusicianId().equals(musician.getMusician().getId()))
                        .findFirst();

                if (position.isEmpty()) {
                    musician.setFormationPositionX(null);
                    musician.setFormationPositionY(null);
                } else {
                    musician.setFormationPositionX(position.get().getFormationPositionX());
                    musician.setFormationPositionY(position.get().getFormationPositionY());
                }
            });
            this.musicianPerformanceRepository.saveAll(musicians);
        }
    }

    @Override
    public void updateRoute(final Long eventId, final EventRouteRequest eventRouteRequest) {
        final var performance = this.performanceRepository.findPerformanceActiveById(eventId);

        if (performance.isEmpty()) {
            throw new NotFoundException("No existe la actuación en la que desea modificar la ruta");
        }

        this.performanceRepository.save(
                this.eventRequestToPerformanceEntityConverter.updateEntityRoute(performance.get(), eventRouteRequest)
        );
    }

    @Override
    public void updateCurrentPosition(final Long eventId, final LatLng latLng) {
        final var performance = this.performanceRepository.findPerformanceActiveById(eventId);

        if (performance.isEmpty()) {
            throw new NotFoundException("No existe la actuación en la que desea modificar la posicion actual");
        }

        this.performanceRepository.save(
                this.eventRequestToPerformanceEntityConverter.updateEntityCurrentPosition(performance.get(), latLng)
        );
    }

}
