package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.application.service.rehearsal.RehearsalService;
import com.feyconsuelo.application.service.repertoire.RepertoireMarchService;
import com.feyconsuelo.application.service.repertoiremarchtype.RepertoireMarchTypeService;
import com.feyconsuelo.application.service.repertoireperformance.RepertoirePerformanceService;
import com.feyconsuelo.application.service.repertoirerehearsal.RepertoireRehearsalService;
import com.feyconsuelo.domain.model.event.EventRepertoireResponse;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchGroupByTypeResponse;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventResponse;
import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;
import com.feyconsuelo.domain.usecase.event.GetEventRepertoire;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetEventRepertoireImpl implements GetEventRepertoire {

    private final RehearsalService rehearsalService;
    private final PerformanceService performanceService;
    private final RepertoireMarchTypeService repertoireMarchTypeService;
    private final RepertoireMarchService repertoireMarchService;
    private final RepertoireRehearsalService repertoireRehearsalService;
    private final RepertoirePerformanceService repertoirePerformanceService;

    @Override
    public Optional<EventRepertoireResponse> execute(final EventTypeEnum eventType, final Long eventId) {

        // obtengo el ultimo ensayo realizado hasta este momento
        if (EventTypeEnum.REHEARSAL.equals(eventType)) {
            final Optional<EventResponse> eventResponse = this.rehearsalService.getById(eventId);

            if (eventResponse.isPresent()) {
                // obtenemos todos los typos y marchas
                final List<RepertoireMarchTypeResponse> types = this.repertoireMarchTypeService.getAll();
                final List<RepertoireMarchResponse> repertoireMarchs = this.repertoireMarchService.getAll();
                final List<RepertoireEventResponse> repertoireEventResponseList = this.repertoireRehearsalService.findAllActivesRepertoireMarchsByRehearsalId(eventResponse.get().getId());

                // asigno a cada marcha si va en el evento o no
                repertoireMarchs.forEach(
                        march -> {
                            march.setChecked(
                                    repertoireEventResponseList.stream()
                                            .anyMatch(repertoireEventResponse -> repertoireEventResponse.getRepertoireMarchResponse().getId().equals(march.getId()))
                            );
                            march.setOrder(
                                    repertoireEventResponseList.stream()
                                            .filter(repertoireEventResponse -> repertoireEventResponse.getRepertoireMarchResponse().getId().equals(march.getId()))
                                            .map(repertoireEventResponse -> repertoireEventResponse.getRepertoireMarchResponse().getOrder())
                                            .findFirst()
                                            .orElse(0)
                            );
                            march.setNumbers(
                                    repertoireEventResponseList.stream()
                                            .filter(repertoireEventResponse -> repertoireEventResponse.getRepertoireMarchResponse().getId().equals(march.getId()))
                                            .map(repertoireEventResponse -> repertoireEventResponse.getRepertoireMarchResponse().getNumbers())
                                            .findFirst()
                                            .orElse(0)
                            );
                        }
                );

                return Optional.of(
                        EventRepertoireResponse.builder()
                                .event(eventResponse.get())
                                .marchs(
                                        types.stream()
                                                .map(
                                                        type -> RepertoireMarchGroupByTypeResponse.builder()
                                                                .type(type)
                                                                .marchs(
                                                                        repertoireMarchs.stream()
                                                                                .filter(
                                                                                        march -> march.getType().getId().equals(type.getId()) &&
                                                                                                (march.getCategory().getCurrent() || march.getChecked())
                                                                                )
                                                                                .toList()
                                                                )
                                                                .build()
                                                )
                                                .filter(repertoireMarchGroupByTypeResponse -> Boolean.FALSE.equals(repertoireMarchGroupByTypeResponse.getMarchs().isEmpty()))
                                                .toList()
                                )
                                .build());
            }
        } else {
            final Optional<EventResponse> eventResponse = this.performanceService.getById(eventId, true);
            if (eventResponse.isPresent()) {
                // obtenemos todos los typos y marchas
                final List<RepertoireMarchTypeResponse> types = this.repertoireMarchTypeService.getAll();
                final List<RepertoireMarchResponse> repertoireMarchs = this.repertoireMarchService.getAll();
                final List<RepertoireEventResponse> repertoireEventResponseList = this.repertoirePerformanceService.findAllActivesRepertoireMarchsByPerformanceId(eventResponse.get().getId());

                // asigno a cada marcha si va en el evento o no
                repertoireMarchs.forEach(
                        march -> {
                            march.setChecked(
                                    repertoireEventResponseList.stream()
                                            .anyMatch(repertoireEventResponse -> repertoireEventResponse.getRepertoireMarchResponse().getId().equals(march.getId()))
                            );
                            march.setOrder(
                                    repertoireEventResponseList.stream()
                                            .filter(repertoireEventResponse -> repertoireEventResponse.getRepertoireMarchResponse().getId().equals(march.getId()))
                                            .map(repertoireEventResponse -> repertoireEventResponse.getRepertoireMarchResponse().getOrder())
                                            .findFirst()
                                            .orElse(0)
                            );
                            march.setNumbers(
                                    repertoireEventResponseList.stream()
                                            .filter(repertoireEventResponse -> repertoireEventResponse.getRepertoireMarchResponse().getId().equals(march.getId()))
                                            .map(repertoireEventResponse -> repertoireEventResponse.getRepertoireMarchResponse().getNumbers())
                                            .findFirst()
                                            .orElse(0)
                            );
                        }
                );

                return Optional.of(
                        EventRepertoireResponse.builder()
                                .event(eventResponse.get())
                                .marchs(
                                        types.stream()
                                                .map(
                                                        type -> RepertoireMarchGroupByTypeResponse.builder()
                                                                .type(type)
                                                                .marchs(
                                                                        repertoireMarchs.stream()
                                                                                .filter(
                                                                                        march -> march.getType().getId().equals(type.getId()) &&
                                                                                                (march.getCategory().getCurrent() || march.getChecked())
                                                                                )
                                                                                .toList()
                                                                )
                                                                .build()
                                                )
                                                .filter(repertoireMarchGroupByTypeResponse -> Boolean.FALSE.equals(repertoireMarchGroupByTypeResponse.getMarchs().isEmpty()))
                                                .toList()
                                )
                                .build());
            }
        }
        return Optional.empty();
    }
}
