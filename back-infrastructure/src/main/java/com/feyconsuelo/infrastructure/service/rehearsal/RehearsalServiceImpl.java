package com.feyconsuelo.infrastructure.service.rehearsal;

import com.feyconsuelo.application.service.rehearsal.RehearsalService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.infrastructure.converter.rehearsal.EventRequestToRehearsalEntityConverter;
import com.feyconsuelo.infrastructure.converter.rehearsal.RehearsalEntityListToEventResponseListConverter;
import com.feyconsuelo.infrastructure.converter.rehearsal.RehearsalEntityToEventResponseConverter;
import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalEntity;
import com.feyconsuelo.infrastructure.repository.RehearsalRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
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

    @Override
    public List<EventResponse> getAll(final LocalDate startDate, final LocalDate endDate) {
        final List<RehearsalEntity> rehearsalList = this.rehearsalRepository.findAllActives(startDate, endDate);
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

}
