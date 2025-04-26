package com.feyconsuelo.infrastructure.converter.performance;

import com.feyconsuelo.domain.model.event.EventCrosshead;
import com.feyconsuelo.domain.model.event.EventCrossheadMarch;
import com.feyconsuelo.domain.model.event.EventCrossheadStreet;
import com.feyconsuelo.infrastructure.entities.performance.CrossheadProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Slf4j
@Component
@RequiredArgsConstructor
public class CrossheadProjectionListToEventCrossheadConverter {

    private final CrossheadPerformanceEntityToEventCrossheadStreetConverter crossheadPerformanceEntityToEventCrossheadStreetConverter;

    public EventCrosshead convert(final List<CrossheadProjection> crossheadProjectionList) {
        // tenemos que agrupar por crossheadperformanceid (ya que esto es cada street), y ahi montar el array de marchas
        // Agrupamos por streetId en un LinkedHashMap para preservar orden
        final Map<Long, List<CrossheadProjection>> grouped = crossheadProjectionList.stream()
                .collect(Collectors.groupingBy(
                        CrossheadProjection::getCrossheadPerformanceId,
                        LinkedHashMap::new,
                        Collectors.toList()
                ));

        // Convertimos cada grupo en un EventCrossheadStreet
        return EventCrosshead.builder()
                .streets(
                        grouped.values().stream()
                                .map(rows -> {
                                    final CrossheadProjection first = rows.get(0);

                                    return EventCrossheadStreet.builder()
                                            .id(first.getCrossheadPerformanceId())
                                            .street(first.getStreet())
                                            .streetOrder(first.getStreetOrder())
                                            .annotations(first.getAnnotations())
                                            .marchs(
                                                    rows.stream()
                                                            .map(r ->
                                                                    EventCrossheadMarch.builder()
                                                                            .id(r.getCrossheadMarchPerformanceId())
                                                                            .marchId(r.getMarchId())
                                                                            .marchName(r.getMarchName())
                                                                            .marchOrder(r.getMarchOrder())
                                                                            .annotations(r.getMarchAnnotations())
                                                                            .build()
                                                            )
                                                            .toList()
                                            )
                                            .build();
                                })
                                .toList()
                )
                .build();

    }
}
