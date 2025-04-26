package com.feyconsuelo.infrastructure.service.rehearsal;

import com.feyconsuelo.application.service.rehearsal.RehearsalService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.event.EventFormationRequest;
import com.feyconsuelo.domain.model.event.EventMusiciansResponse;
import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.musician.MusicianFormationRequest;
import com.feyconsuelo.infrastructure.converter.rehearsal.EventRequestToRehearsalEntityConverter;
import com.feyconsuelo.infrastructure.converter.rehearsal.RehearsalEntityListToEventResponseListConverter;
import com.feyconsuelo.infrastructure.converter.rehearsal.RehearsalEntityToEventResponseConverter;
import com.feyconsuelo.infrastructure.converter.rehearsal.RehearsalMusiciansProjectionToEventMusiciansResponseConverter;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalEntity;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalPK;
import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalEntity;
import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalMusiciansProjection;
import com.feyconsuelo.infrastructure.repository.MusicianRehearsalRepository;
import com.feyconsuelo.infrastructure.repository.RehearsalRepository;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class RehearsalServiceImpl implements RehearsalService {

    private final RehearsalRepository rehearsalRepository;
    private final EventRequestToRehearsalEntityConverter eventRequestToRehearsalEntityConverter;
    private final RehearsalEntityListToEventResponseListConverter rehearsalEntityListToEventResponseListConverter;
    private final RehearsalEntityToEventResponseConverter rehearsalEntityToEventResponseConverter;
    private final MusicianRehearsalRepository musicianRehearsalRepository;
    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;
    private final RehearsalMusiciansProjectionToEventMusiciansResponseConverter rehearsalMusiciansProjectionToEventMusiciansResponseConverter;

    @Override
    public List<EventResponse> getAll(final LocalDate startDate, final LocalDate endDate) {
        final List<RehearsalEntity> rehearsalList = this.rehearsalRepository.findAllActives(
                startDate,
                endDate,
                startDate == null,
                endDate == null
        );
        return this.rehearsalEntityListToEventResponseListConverter.convert(rehearsalList);
    }

    @Override
    public Optional<EventResponse> getById(final Long eventId) {
        final var event = this.rehearsalRepository.findRehearsalActiveById(eventId);
        return event.map(this.rehearsalEntityToEventResponseConverter::convert);
    }

    @Override
    public Optional<EventResponse> getByDate(final LocalDate date) {
        final var event = this.rehearsalRepository.findRehearsalActiveByDate(date);
        return event.map(this.rehearsalEntityToEventResponseConverter::convert);
    }

    @Override
    public void insert(final EventRequest eventRequest) {
        this.rehearsalRepository.save(
                this.eventRequestToRehearsalEntityConverter.convert(eventRequest)
        );
    }

    @Override
    public void update(final Long eventId, final EventRequest eventRequest) {
        final var rehearsal = this.rehearsalRepository.findRehearsalActiveById(eventId);

        if (rehearsal.isEmpty()) {
            throw new NotFoundException("No existe el ensayo que desea modificar");
        }

        this.rehearsalRepository.save(
                this.eventRequestToRehearsalEntityConverter.updateEntity(rehearsal.get(), eventRequest)
        );
    }

    @Override
    public void delete(final Long eventId) {
        this.rehearsalRepository.deleteById(eventId);
    }

    @Override
    public void logicalDelete(final Long eventId) {
        final var event = this.rehearsalRepository.findRehearsalActiveById(eventId);

        if (event.isEmpty()) {
            throw new NotFoundException("No existe el ensayo que desea eliminar");
        }

        this.rehearsalRepository.save(this.eventRequestToRehearsalEntityConverter.deleteEntity(event.get()));
    }

    @Override
    public Optional<EventResponse> findLastRehearsalUntilDateTime(final LocalDateTime dateTime) {
        final var event = this.rehearsalRepository.findLastRehearsalUntilDateTime(dateTime);
        return event.map(this.rehearsalEntityToEventResponseConverter::convert);
    }

    @Override
    @Transactional
    public void updateFormation(final Long eventId, final EventFormationRequest eventFormationRequest) {
        // despues uno a uno vamos actualizando su posicion
        final List<MusicianRehearsalEntity> musiciansRehesalEntityList = this.musicianRehearsalRepository.findAllActivesMusiciansByRehearsalId(eventId);

        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(musiciansRehesalEntityList))) {
            musiciansRehesalEntityList.forEach(musician -> {
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
            this.musicianRehearsalRepository.saveAll(musiciansRehesalEntityList);
        }

        // siguiente paso es eliminar todos los musicos con id negativo, y meter los nuevos que vienen con id negativo
        this.musicianRehearsalRepository.deleteFakeMusicians(eventId);

        // ahora metemos todos los que traen id negativo
        for (final MusicianFormationRequest musicianFormationRequest : eventFormationRequest.getMusicians()) {
            if (musicianFormationRequest.getMusicianId() < 0) {
                this.musicianRehearsalRepository.save(
                        MusicianRehearsalEntity.builder()
                                .id(
                                        MusicianRehearsalPK.builder()
                                                .rehearsalId(eventId)
                                                .musicianId(musicianFormationRequest.getMusicianId())
                                                .build()
                                )
                                .formationPositionX(musicianFormationRequest.getFormationPositionX())
                                .formationPositionY(musicianFormationRequest.getFormationPositionY())
                                .updateUserMR(this.tokenInfoExtractorService.getUsername())
                                .build()
                );
            }
        }
    }

    @Override
    public Optional<EventMusiciansResponse> getEventMusicians(final Long eventId) {
        final Optional<RehearsalMusiciansProjection> eventMusicians = this.rehearsalRepository.findRehearsalMusicians(eventId);
        return eventMusicians.map(this.rehearsalMusiciansProjectionToEventMusiciansResponseConverter::convert);
    }

}
