package com.feyconsuelo.infrastructure.service.performance;

import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.event.EventCrosshead;
import com.feyconsuelo.domain.model.event.EventCurrentDataResponse;
import com.feyconsuelo.domain.model.event.EventFormationRequest;
import com.feyconsuelo.domain.model.event.EventMusiciansResponse;
import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventRouteRequest;
import com.feyconsuelo.domain.model.event.EventRouteResponse;
import com.feyconsuelo.domain.model.event.LatLng;
import com.feyconsuelo.domain.model.musician.MusicianFormationRequest;
import com.feyconsuelo.infrastructure.converter.musicianperformance.MusicianPerformanceProjectionListToEventResponseListConverter;
import com.feyconsuelo.infrastructure.converter.performance.CrossheadPerformanceEntityListToEventCrossheadConverter;
import com.feyconsuelo.infrastructure.converter.performance.CrossheadProjectionListToEventCrossheadConverter;
import com.feyconsuelo.infrastructure.converter.performance.EventCrossheadToCrossheadPerformanceEntityListConverter;
import com.feyconsuelo.infrastructure.converter.performance.EventRequestToPerformanceEntityConverter;
import com.feyconsuelo.infrastructure.converter.performance.PerformanceEntityListToEventResponseListConverter;
import com.feyconsuelo.infrastructure.converter.performance.PerformanceEntityToEventCurrentDataResponseConverter;
import com.feyconsuelo.infrastructure.converter.performance.PerformanceEntityToEventResponseConverter;
import com.feyconsuelo.infrastructure.converter.performance.PerformanceEntityToEventRouteResponseConverter;
import com.feyconsuelo.infrastructure.converter.performance.PerformanceEntityToLatLngConverter;
import com.feyconsuelo.infrastructure.converter.performance.PerformanceMusiciansProjectionToEventMusiciansResponseConverter;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceEntity;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformancePK;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceProjection;
import com.feyconsuelo.infrastructure.entities.performance.CrossheadMarchPerformanceEntity;
import com.feyconsuelo.infrastructure.entities.performance.CrossheadPerformanceEntity;
import com.feyconsuelo.infrastructure.entities.performance.CrossheadProjection;
import com.feyconsuelo.infrastructure.entities.performance.PerformanceEntity;
import com.feyconsuelo.infrastructure.entities.performance.PerformanceMusiciansProjection;
import com.feyconsuelo.infrastructure.repository.CrossheadMarchPerformanceRepository;
import com.feyconsuelo.infrastructure.repository.CrossheadPerformanceRepository;
import com.feyconsuelo.infrastructure.repository.MusicianPerformanceRepository;
import com.feyconsuelo.infrastructure.repository.PerformanceRepository;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.time.LocalDate;
import java.util.ArrayList;
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
    private final PerformanceEntityToEventRouteResponseConverter performanceEntityToEventRouteResponseConverter;
    private final PerformanceEntityToLatLngConverter performanceEntityToLatLngConverter;
    private final PerformanceEntityToEventCurrentDataResponseConverter performanceEntityToEventCurrentDataResponseConverter;
    private final CrossheadPerformanceRepository crossheadPerformanceRepository;
    private final CrossheadPerformanceEntityListToEventCrossheadConverter crossheadPerformanceEntityListToEventCrossheadConverter;
    private final EventCrossheadToCrossheadPerformanceEntityListConverter eventCrossheadToCrossheadPerformanceEntityListConverter;
    private final CrossheadMarchPerformanceRepository crossheadMarchPerformanceRepository;
    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;
    private final CrossheadProjectionListToEventCrossheadConverter crossheadProjectionListToEventCrossheadConverter;
    private final MusicianPerformanceProjectionListToEventResponseListConverter musicianPerformanceProjectionListToEventResponseListConverter;
    private final PerformanceMusiciansProjectionToEventMusiciansResponseConverter performanceMusiciansProjectionToEventMusiciansResponseConverter;

    @Override
    public List<EventResponse> getAll(final LocalDate startDate, final LocalDate endDate) {
        if (startDate != null && endDate != null && startDate.isEqual(endDate)) {
            final List<PerformanceEntity> performanceList = this.performanceRepository.findAllActivesWithImages(
                    startDate,
                    endDate,
                    Boolean.FALSE,
                    Boolean.FALSE
            );
            return this.performanceEntityListToEventResponseListConverter.convert(
                    performanceList,
                    Boolean.FALSE
            );
        } else {
            final List<MusicianPerformanceProjection> performanceList = this.performanceRepository.findAllActivesWithoutImages(
                    startDate,
                    endDate,
                    startDate == null,
                    endDate == null
            );
            return this.musicianPerformanceProjectionListToEventResponseListConverter.convert(performanceList);
        }

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
    @Transactional
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

        // siguiente paso es eliminar todos los musicos con id negativo, y meter los nuevos que vienen con id negativo
        this.musicianPerformanceRepository.deleteFakeMusicians(eventId);

        // ahora metemos todos los que traen id negativo
        for (final MusicianFormationRequest musicianFormationRequest : eventFormationRequest.getMusicians()) {
            if (musicianFormationRequest.getMusicianId() < 0) {
                this.musicianPerformanceRepository.save(
                        MusicianPerformanceEntity.builder()
                                .id(
                                        MusicianPerformancePK.builder()
                                                .performanceId(eventId)
                                                .musicianId(musicianFormationRequest.getMusicianId())
                                                .build()
                                )
                                .bus(false) // para los ficticios este dato da igual
                                .formationPositionX(musicianFormationRequest.getFormationPositionX())
                                .formationPositionY(musicianFormationRequest.getFormationPositionY())
                                .updateUserMP(this.tokenInfoExtractorService.getUsername())
                                .build()
                );
            }
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

    @Override
    public Optional<LatLng> getCurrentPosition(final Long eventId) {
        final var event = this.performanceRepository.findPerformanceActiveById(eventId);
        return event.map(this.performanceEntityToLatLngConverter::convert);
    }

    @Override
    public Optional<EventRouteResponse> getRoute(final Long eventId) {
        final var event = this.performanceRepository.findPerformanceActiveById(eventId);
        return event.map(this.performanceEntityToEventRouteResponseConverter::convert);
    }

    @Override
    public void updateCurrentMarch(final Long eventId, final String march) {
        final var performance = this.performanceRepository.findPerformanceActiveById(eventId);

        if (performance.isEmpty()) {
            throw new NotFoundException("No existe la actuación en la que desea modificar la posicion actual");
        }

        this.performanceRepository.save(
                this.eventRequestToPerformanceEntityConverter.updateEntityCurrentMarch(performance.get(), march)
        );
    }

    @Override
    public Optional<EventCurrentDataResponse> getCurrentData(final Long eventId) {
        final var event = this.performanceRepository.findPerformanceActiveById(eventId);
        return event.map(this.performanceEntityToEventCurrentDataResponseConverter::convert);
    }

    @Override
    public Optional<EventCrosshead> getCrosshead(final Long eventId) {
        final List<CrossheadProjection> crossheadProjectionList = this.crossheadPerformanceRepository.findCrossheadByPerformanceId(eventId);
        if (CollectionUtils.isEmpty(crossheadProjectionList)) {
            return Optional.empty();
        } else {
            return Optional.of(this.crossheadProjectionListToEventCrossheadConverter.convert(crossheadProjectionList));
        }
    }

    @Override
    @Transactional
    public void updateCrosshead(final Long eventId, final EventCrosshead eventCrosshead) {
        // eliminamos los antiguos
        this.crossheadPerformanceRepository.deleteCrossheadByPerformanceId(eventId);

        // insertamos de nuevo
        final List<CrossheadPerformanceEntity> crossheadPerformanceEntityList = this.eventCrossheadToCrossheadPerformanceEntityListConverter.convert(eventCrosshead, eventId);
        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(crossheadPerformanceEntityList))) {
            for (final CrossheadPerformanceEntity crossheadPerformanceEntity : crossheadPerformanceEntityList) {
                final List<CrossheadMarchPerformanceEntity> marchs = new ArrayList<>(crossheadPerformanceEntity.getMarchs());
                crossheadPerformanceEntity.setMarchs(null);
                final CrossheadPerformanceEntity insertEntity = this.crossheadPerformanceRepository.save(crossheadPerformanceEntity);

                if (Boolean.FALSE.equals(CollectionUtils.isEmpty(marchs))) {
                    for (final CrossheadMarchPerformanceEntity march : marchs) {
                        march.setCrosshead(insertEntity);
                        march.setCrossheadId(insertEntity.getId());
                    }
                    this.crossheadMarchPerformanceRepository.saveAll(marchs);
                }
            }
        }
        this.crossheadPerformanceRepository.saveAll(crossheadPerformanceEntityList);
    }

    @Override
    public Optional<EventMusiciansResponse> getEventMusicians(final Long eventId, final Boolean isThumbnail, final Boolean route) {
        final Optional<PerformanceMusiciansProjection> eventMusicians = this.performanceRepository.findPerformanceMusicians(eventId);
        return eventMusicians.map(ev -> this.performanceMusiciansProjectionToEventMusiciansResponseConverter.convert(ev, route));
    }
}
