package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.service.repertoireperformance.RepertoirePerformanceService;
import com.feyconsuelo.application.service.repertoirerehearsal.RepertoireRehearsalService;
import com.feyconsuelo.application.service.user.TokenInfoExtractorService;
import com.feyconsuelo.domain.model.event.EventRepertoireResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.user.UserRoleEnum;
import com.feyconsuelo.domain.usecase.event.GetEventRepertoire;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetEventRepertoireImpl implements GetEventRepertoire {

    private final RepertoireRehearsalService repertoireRehearsalService;
    private final RepertoirePerformanceService repertoirePerformanceService;
    private final TokenInfoExtractorService tokenInfoExtractorService;

    @SuppressWarnings("java:S3776")
    @Override
    public Optional<EventRepertoireResponse> execute(final EventTypeEnum eventType, final Long eventId) {

        // obtengo el ultimo ensayo realizado hasta este momento
        if (EventTypeEnum.REHEARSAL.equals(eventType)) {
            final Optional<EventRepertoireResponse> eventRepertoireResponse = this.repertoireRehearsalService.getEventRepertoireRehearsal(eventId);
            // recorremos todas las marchas si hay asignandole el tipo
            if (eventRepertoireResponse.isPresent() && !CollectionUtils.isEmpty(eventRepertoireResponse.get().getMarchs())) {
                eventRepertoireResponse.get().getMarchs().forEach(type -> {
                    if (!CollectionUtils.isEmpty(type.getMarchs())) {
                        // recorremos todas las marchas y asignamos el type
                        type.getMarchs().forEach(march -> march.setType(type.getType()));
                    }
                });

                // ahora debemos eliminar types que no tengan marchas
                eventRepertoireResponse.get().setMarchs(
                        eventRepertoireResponse.get().getMarchs().stream()
                                .filter(repertoireMarchGroupByTypeResponse -> !CollectionUtils.isEmpty(repertoireMarchGroupByTypeResponse.getMarchs()))
                                .toList()
                );
            }
            return eventRepertoireResponse;
        } else {
            final Optional<EventRepertoireResponse> eventRepertoireResponse = this.repertoirePerformanceService.getEventRepertoireRehearsal(eventId);

            // recorremos todas las marchas si hay asignandole el tipo
            if (eventRepertoireResponse.isPresent() && !CollectionUtils.isEmpty(eventRepertoireResponse.get().getMarchs())) {
                eventRepertoireResponse.get().getMarchs().forEach(type -> {
                    if (!CollectionUtils.isEmpty(type.getMarchs())) {
                        // recorremos todas las marchas y asignamos el type
                        type.getMarchs().forEach(
                                march -> {
                                    march.setType(type.getType());
                                    // ademas si el repertorio no es publico y no es administrador, debemos desmarcar las marchas seleccionadas
                                    if (Boolean.FALSE.equals(this.tokenInfoExtractorService.hasRole(UserRoleEnum.SUPER_ADMIN.getId())) &&
                                            Boolean.FALSE.equals(eventRepertoireResponse.get().getEvent().getRepertoirePublic())) {
                                        march.setNumbers(0);
                                        march.setChecked(Boolean.FALSE);
                                    }
                                }
                        );
                    }
                });

                // ahora debemos eliminar types que no tengan marchas
                eventRepertoireResponse.get().setMarchs(
                        eventRepertoireResponse.get().getMarchs().stream()
                                .filter(repertoireMarchGroupByTypeResponse -> !CollectionUtils.isEmpty(repertoireMarchGroupByTypeResponse.getMarchs()))
                                .toList()
                );
            }
            return eventRepertoireResponse;
        }
    }
}
