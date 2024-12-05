package com.feyconsuelo.infrastructure.service.performance;

import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.infrastructure.converter.performance.EventRequestToPerformanceEntityConverter;
import com.feyconsuelo.infrastructure.converter.performance.PerformanceEntityListToEventResponseListConverter;
import com.feyconsuelo.infrastructure.converter.performance.PerformanceEntityToEventResponseConverter;
import com.feyconsuelo.infrastructure.entities.performance.PerformanceEntity;
import com.feyconsuelo.infrastructure.repository.PerformanceRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

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

    @Override
    public List<EventResponse> getAll(final LocalDate startDate, final LocalDate endDate) {
        final List<PerformanceEntity> performanceList = this.performanceRepository.findAllActives(startDate, endDate);
        return this.performanceEntityListToEventResponseListConverter.convert(performanceList);
    }

    @Override
    public Optional<EventResponse> getById(final Long eventId) {
        final var event = this.performanceRepository.findPerformanceActiveById(eventId);
        return event.map(this.performanceEntityToEventResponseConverter::convert);
    }

    @Override
    public Optional<EventResponse> getByDate(final LocalDate date) {
        final var event = this.performanceRepository.findPerformanceActiveByDate(date);
        return event.map(this.performanceEntityToEventResponseConverter::convert);
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

}
