package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.usecase.performance.GetAllPerformanceImpl;
import com.feyconsuelo.application.usecase.rehearsal.GetAllRehearsalImpl;
import com.feyconsuelo.application.usecase.utils.NumberService;
import com.feyconsuelo.domain.model.event.EventDetailStatsResponse;
import com.feyconsuelo.domain.model.event.EventMarchStatsResponse;
import com.feyconsuelo.domain.model.event.EventMarchTypeStatsResponse;
import com.feyconsuelo.domain.model.event.EventRepertoireResponse;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventStatInfoResponse;
import com.feyconsuelo.domain.model.event.EventStatsResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.event.GlobalEventStatsResponse;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.domain.usecase.event.GetEventStats;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Component
@RequiredArgsConstructor
@SuppressWarnings("java:S1192")
public class GetEventStatsImpl implements GetEventStats {

    private static final String MARCH_CATEGORY_SLOW = "LENTO";
    private static final String MARCH_CATEGORY_RAPID = "ORDINARIO";
    private static final String MARCH_TYPE_OWN = "PROPIA";
    private static final List<String> MARCH_TYPE_EXCLUDE_MOST_AND_LEAST = List.of("HIMNO", "ORDINARIO", "NAVIDAD");
    private final GetAllPerformanceImpl getAllPerformance;
    private final GetAllRehearsalImpl getAllRehearsal;
    private final GetEventRepertoireImpl getEventRepertoire;
    private final NumberService numberService;

    @Override
    @SuppressWarnings("java:S3776")
    public Optional<GlobalEventStatsResponse> execute(final Boolean excludeSpecialTypes,
                                                      final EventTypeEnum eventType,
                                                      final LocalDate startDate,
                                                      final LocalDate endDate) {
        final List<EventResponse> performanceList = EventTypeEnum.PERFORMANCE.equals(eventType) || eventType == null ?
                this.getAllPerformance.execute(startDate, endDate, Optional.empty(), null) :
                List.of();

        final List<EventResponse> rehearsalList = EventTypeEnum.REHEARSAL.equals(eventType) || eventType == null ?
                this.getAllRehearsal.execute(startDate, endDate, Optional.empty()) :
                List.of();

        // unimos ambos arrays con stream concat
        final List<EventResponse> eventList = Stream.of(performanceList, rehearsalList)
                .flatMap(List::stream)
                .toList();

        if (CollectionUtils.isEmpty(eventList)) {
            return Optional.empty();
        }

        final List<EventStatsResponse> eventStats = new ArrayList<>();
        for (final EventResponse eventResponse : eventList) {
            final Optional<EventRepertoireResponse> repertoire = this.getEventRepertoire.execute(eventResponse.getType(), eventResponse.getId(), Boolean.FALSE);
            final List<EventMarchStatsResponse> marchsStats;
            final List<EventMarchTypeStatsResponse> marchsTypeStats;
            final int totalNumberMarchs;
            final Integer totalNumberMarchsOwnSlow;
            final Integer totalNumberMarchsOtherSlow;
            final Integer totalNumberMarchsOrdinary;
            final double totalNumberHours;
            final double totalNumberKilometers;
            final double averageNumberMarchsByHour;
            final double percentageNumberMarchsOwn;
            final double percentageNumberMarchsOther;
            if (repertoire.isEmpty() || CollectionUtils.isEmpty(repertoire.get().getMarchs())) {
                marchsStats = List.of();
                marchsTypeStats = List.of();
                totalNumberMarchs = 0;
                totalNumberMarchsOwnSlow = 0;
                totalNumberMarchsOtherSlow = 0;
                totalNumberMarchsOrdinary = 0;
                totalNumberHours = 0.0;
                totalNumberKilometers = 0.0;
                averageNumberMarchsByHour = 0.0;
                percentageNumberMarchsOwn = 0.0;
                percentageNumberMarchsOther = 0.0;
            } else {
                marchsStats = repertoire.get().getMarchs().stream()
                        .flatMap(march -> march.getMarchs().stream())
                        //.filter(march -> march.getNumbers() > 0)
                        .map(march -> EventMarchStatsResponse.builder()
                                .name(march.getName())
                                .count(march.getNumbers())
                                .orderType(march.getType().getOrder())
                                .type(march.getType().getName())
                                .category(march.getCategory().getName())
                                .image(march.getType().getImage())
                                .build()
                        )
                        .sorted(
                                Comparator.comparing(EventMarchStatsResponse::getOrderType)
                                        .thenComparing(EventMarchStatsResponse::getName)
                        )
                        .toList();

                // ya vienen agrupadas por tipoo, asi que tengo que contar el numero todal de cada tipo
                marchsTypeStats = repertoire.get().getMarchs().stream()
                        .map(type -> EventMarchTypeStatsResponse.builder()
                                .name(type.getType().getName())
                                .count(type.getMarchs().stream().map(RepertoireMarchResponse::getNumbers).reduce(0, Integer::sum))
                                .orderType(type.getType().getOrder())
                                .mostPlayerMarch(
                                        type.getMarchs().stream()
                                                .filter(m -> m.getNumbers().equals(
                                                        type.getMarchs().stream()
                                                                .map(RepertoireMarchResponse::getNumbers)
                                                                .max(Integer::compareTo)
                                                                .orElse(0)
                                                ))
                                                .map(m -> EventMarchStatsResponse.builder()
                                                        .name(m.getName())
                                                        .count(m.getNumbers())
                                                        .orderType(m.getType().getOrder())
                                                        .type(m.getType().getName())
                                                        .category(m.getCategory().getName())
                                                        .image(m.getType().getImage())
                                                        .build()
                                                )
                                                .toList()
                                )
                                .leastPlayerMarch(
                                        type.getMarchs().stream()
                                                .filter(m -> m.getNumbers().equals(
                                                        type.getMarchs().stream()
                                                                .map(RepertoireMarchResponse::getNumbers)
                                                                .min(Integer::compareTo)
                                                                .orElse(0)
                                                ))
                                                .map(m -> EventMarchStatsResponse.builder()
                                                        .name(m.getName())
                                                        .count(m.getNumbers())
                                                        .orderType(m.getType().getOrder())
                                                        .type(m.getType().getName())
                                                        .category(m.getCategory().getName())
                                                        .image(m.getType().getImage())
                                                        .build()
                                                )
                                                .toList()
                                )
                                .image(type.getType().getImage())
                                .build()
                        )
                        .toList();

                totalNumberMarchs = marchsStats.stream().map(EventMarchStatsResponse::getCount).reduce(0, Integer::sum);
                totalNumberMarchsOwnSlow = this.getNumberMarchCategoryType(repertoire.get(), MARCH_CATEGORY_SLOW, MARCH_TYPE_OWN);
                totalNumberMarchsOtherSlow = this.getNumberMarchCategoryNotType(repertoire.get(), MARCH_CATEGORY_SLOW, MARCH_TYPE_OWN);
                final int totalNumberMarchsSlow = totalNumberMarchsOwnSlow + totalNumberMarchsOtherSlow;
                totalNumberMarchsOrdinary = this.getNumberMarchCategoryType(repertoire.get(), MARCH_CATEGORY_RAPID, "");
                totalNumberHours = totalNumberMarchs > 0 ? this.numberService.round(this.getEventDuration(eventResponse)) : 0.0;
                totalNumberKilometers = eventResponse.getKilometers() != null && totalNumberMarchs > 0 ? this.numberService.round(eventResponse.getKilometers()) : 0.0;
                averageNumberMarchsByHour = totalNumberHours > 0 ? this.numberService.round(totalNumberMarchs / totalNumberHours, 2) : 0.0;
                percentageNumberMarchsOwn = totalNumberMarchsSlow > 0 ? this.numberService.round((totalNumberMarchsOwnSlow * 100.0) / totalNumberMarchsSlow, 2) : 0.0;
                percentageNumberMarchsOther = totalNumberMarchsSlow > 0 ? this.numberService.round((totalNumberMarchsOtherSlow * 100.0) / totalNumberMarchsSlow, 2) : 0.0;
            }

            final List<EventMarchStatsResponse> mostPlayerMarchOwnSlow = this.getMostPlayerMarchType(marchsStats, MARCH_TYPE_OWN, excludeSpecialTypes);
            final List<EventMarchStatsResponse> mostPlayerMarchOtherSlow = this.getMostPlayerMarchNotType(marchsStats, MARCH_TYPE_OWN, excludeSpecialTypes);

            final List<EventMarchStatsResponse> leastPlayerMarchOwnSlow = this.getLeastPlayerMarchType(marchsStats, MARCH_TYPE_OWN, excludeSpecialTypes);
            final List<EventMarchStatsResponse> leastPlayerMarchOtherSlow = this.getLeastPlayerMarchNotType(marchsStats, MARCH_TYPE_OWN, excludeSpecialTypes);

            // por evento
            eventStats.add(
                    EventStatsResponse.builder()
                            .event(
                                    EventStatInfoResponse.builder()
                                            .id(eventResponse.getId())
                                            .type(eventResponse.getType())
                                            .title(eventResponse.getTitle())
                                            .performanceType(eventResponse.getPerformanceType())
                                            .description(eventResponse.getDescription())
                                            .date(eventResponse.getDate())
                                            .endTime(eventResponse.getEndTime())
                                            .startTime(eventResponse.getStartTime())
                                            .province(eventResponse.getProvince())
                                            .municipality(eventResponse.getMunicipality())
                                            .image(eventResponse.getImage())
                                            .duration(eventResponse.getDuration())
                                            .kilometers(eventResponse.getKilometers())
                                            .build()
                            )
                            .stats(
                                    EventDetailStatsResponse.builder()
                                            .marchsStats(
                                                    marchsStats.stream()
                                                            .filter(march -> Boolean.FALSE.equals(this.isExclude(march.getType().toUpperCase(), excludeSpecialTypes)))
                                                            .sorted(
                                                                    Comparator.comparing(EventMarchStatsResponse::getOrderType)
                                                                            .thenComparing(EventMarchStatsResponse::getName)
                                                            )
                                                            .toList()
                                            )
                                            .marchsTypeStats(
                                                    marchsTypeStats.stream()
                                                            .filter(type -> Boolean.FALSE.equals(this.isExclude(type.getName().toUpperCase(), excludeSpecialTypes)))
                                                            .sorted(
                                                                    Comparator.comparing(EventMarchTypeStatsResponse::getOrderType)
                                                            )
                                                            .toList()
                                            )
                                            .mostPlayerMarchOtherSlow(mostPlayerMarchOtherSlow)
                                            .mostPlayerMarchOwnSlow(mostPlayerMarchOwnSlow)
                                            .leastPlayerMarchOtherSlow(leastPlayerMarchOtherSlow)
                                            .leastPlayerMarchOwnSlow(leastPlayerMarchOwnSlow)
                                            .countMostPlayerMarch(Stream.of(mostPlayerMarchOwnSlow, mostPlayerMarchOtherSlow).flatMap(List::stream).max(Comparator.comparing(EventMarchStatsResponse::getCount)).map(EventMarchStatsResponse::getCount).orElse(0))
                                            .totalNumberMarchs(totalNumberMarchs)
                                            .totalNumberMarchsOwnSlow(totalNumberMarchsOwnSlow)
                                            .totalNumberMarchsOtherSlow(totalNumberMarchsOtherSlow)
                                            .totalNumberMarchsOrdinary(totalNumberMarchsOrdinary)
                                            .totalNumberHours(totalNumberHours)
                                            .totalNumberKilometers(totalNumberKilometers)
                                            .averageNumberMarchsByHour(averageNumberMarchsByHour)
                                            .percentageNumberMarchsOwn(percentageNumberMarchsOwn)
                                            .percentageNumberMarchsOther(percentageNumberMarchsOther)
                                            .build()
                            )
                            .build()
            );
        }

        // ahora para cada evente tenemos que obtener las estadisticas de marchas
        // el array de marchas sera agrupar todos los arrays de marchas de los eventos y sumar las marchas que coincidan por nombre
        final List<EventMarchStatsResponse> globalMarchsStats = eventStats.stream()
                .flatMap(event -> event.getStats().getMarchsStats().stream()
                        // Creamos un objeto nuevo a partir de la información de 'march'
                        .map(march -> EventMarchStatsResponse.builder()
                                .name(march.getName())
                                .count(march.getCount())
                                .orderType(march.getOrderType())
                                .type(march.getType())
                                .category(march.getCategory())
                                .image(march.getImage())
                                .build()
                        )
                )
                .collect(
                        ArrayList::new,
                        (list, march) -> {
                            // Aquí 'march' ya es un objeto NUEVO, distinto al de la lista original
                            final Optional<EventMarchStatsResponse> existing = list.stream()
                                    .filter(m -> m.getName().equals(march.getName()))
                                    .findFirst();
                            if (existing.isEmpty()) {
                                list.add(march);
                            } else {
                                existing.get().setCount(existing.get().getCount() + march.getCount());
                            }
                        },
                        ArrayList::addAll
                );

        // a partir de globalMarchStats, formamos un array agrupando por type y sumando las marchas que coincidan por nombre de tipo y  ademas formando dentro de cada type un array con todas las marchas agrupàdas por nombre y sumando el count
        final Map<String, List<EventMarchStatsResponse>> groupedByType = globalMarchsStats.stream()
                .filter(march -> Boolean.FALSE.equals(this.isExclude(march.getType().toUpperCase(), excludeSpecialTypes)))
                .collect(Collectors.groupingBy(EventMarchStatsResponse::getType));
        final List<EventMarchTypeStatsResponse> globalMarchsTypeStats = new ArrayList<>();

        // Procesar cada grupo
        groupedByType.forEach((type, statsList) -> {
            // Sumar los count para obtener el total del grupo
            final int totalCount = statsList.stream()
                    .mapToInt(EventMarchStatsResponse::getCount)
                    .sum();

            // Encontrar el valor máximo de count dentro del grupo
            final int maxCount = statsList.stream()
                    .mapToInt(EventMarchStatsResponse::getCount)
                    .max()
                    .orElse(0);

            final int minCount = statsList.stream()
                    .mapToInt(EventMarchStatsResponse::getCount)
                    .min()
                    .orElse(0);

            // Filtrar las marchas que tengan el count máximo
            final List<EventMarchStatsResponse> mostPlayerMarch = statsList.stream()
                    .filter(stat -> stat.getCount() == maxCount)
                    .toList();

            // Filtrar las marchas que tengan el count máximo
            final List<EventMarchStatsResponse> leastPlayerMarch = statsList.stream()
                    .filter(stat -> stat.getCount() == minCount)
                    .toList();

            // Seleccionamos la imagen y orderType del primer elemento del grupo (puedes ajustar la lógica según necesites)
            final EventMarchStatsResponse firstElement = statsList.get(0);

            // Crear el objeto de respuesta para el tipo de marcha
            final EventMarchTypeStatsResponse typeStats = EventMarchTypeStatsResponse.builder().build();
            typeStats.setName(type);  // se asigna el tipo al campo 'name'
            typeStats.setCount(totalCount);
            typeStats.setImage(firstElement.getImage());
            typeStats.setOrderType(firstElement.getOrderType());
            typeStats.setMostPlayerMarch(mostPlayerMarch);
            typeStats.setLeastPlayerMarch(leastPlayerMarch);

            globalMarchsTypeStats.add(typeStats);
        });

        final Integer globalTotalNumberMarchs = eventStats.stream().map(event -> event.getStats().getTotalNumberMarchs()).reduce(0, Integer::sum);
        final Integer globalTotalNumberMarchsOwnSlow = eventStats.stream().map(event -> event.getStats().getTotalNumberMarchsOwnSlow()).reduce(0, Integer::sum);
        final Integer globalTotalNumberMarchsOtherSlow = eventStats.stream().map(event -> event.getStats().getTotalNumberMarchsOtherSlow()).reduce(0, Integer::sum);
        final Integer globalTotalNumberMarchsOrdinary = eventStats.stream().map(event -> event.getStats().getTotalNumberMarchsOrdinary()).reduce(0, Integer::sum);
        final Double globalTotalNumberHours = eventStats.stream().map(event -> event.getStats().getTotalNumberHours()).reduce(0.0, Double::sum);
        final Double globalTotalNumberKilometers = eventStats.stream().map(event -> event.getStats().getTotalNumberKilometers()).reduce(0.0, Double::sum);
        final Double globalAverageNumberMarchsByHour = globalTotalNumberMarchs > 0 ? this.numberService.round(globalTotalNumberMarchs / globalTotalNumberHours, 2) : 0.0;
        final Double globalPercentageNumberMarchsOwn = globalTotalNumberMarchsOwnSlow > 0 ? this.numberService.round((globalTotalNumberMarchsOwnSlow * 100.0) / (globalTotalNumberMarchsOwnSlow + globalTotalNumberMarchsOtherSlow), 2) : 0.0;
        final Double globalPercentageNumberMarchsOther = globalTotalNumberMarchsOtherSlow > 0 ? this.numberService.round((globalTotalNumberMarchsOtherSlow * 100.0) / (globalTotalNumberMarchsOwnSlow + globalTotalNumberMarchsOtherSlow), 2) : 0.0;

        final List<EventMarchStatsResponse> mostPlayerMarchOwnSlow = this.getMostPlayerMarchType(globalMarchsStats, MARCH_TYPE_OWN, excludeSpecialTypes);
        final List<EventMarchStatsResponse> mostPlayerMarchOtherSlow = this.getMostPlayerMarchNotType(globalMarchsStats, MARCH_TYPE_OWN, excludeSpecialTypes);

        final List<EventMarchStatsResponse> leastlayerMarchOwnSlow = this.getLeastPlayerMarchType(globalMarchsStats, MARCH_TYPE_OWN, excludeSpecialTypes);
        final List<EventMarchStatsResponse> leastPlayerMarchOtherSlow = this.getLeastPlayerMarchNotType(globalMarchsStats, MARCH_TYPE_OWN, excludeSpecialTypes);

        return Optional.of(
                GlobalEventStatsResponse.builder()
                        .totalStats(
                                EventDetailStatsResponse.builder()
                                        .marchsStats(
                                                globalMarchsStats.stream()
                                                        .filter(march -> Boolean.FALSE.equals(this.isExclude(march.getType().toUpperCase(), excludeSpecialTypes)))
                                                        .sorted(
                                                                Comparator.comparing(EventMarchStatsResponse::getOrderType)
                                                                        .thenComparing(EventMarchStatsResponse::getName)
                                                        )
                                                        .toList()
                                        )
                                        .marchsTypeStats(
                                                globalMarchsTypeStats.stream()
                                                        .filter(type -> Boolean.FALSE.equals(this.isExclude(type.getName().toUpperCase(), excludeSpecialTypes)))
                                                        .sorted(
                                                                Comparator.comparing(EventMarchTypeStatsResponse::getOrderType)
                                                        )
                                                        .toList()
                                        )
                                        .mostPlayerMarchOwnSlow(mostPlayerMarchOwnSlow)
                                        .mostPlayerMarchOtherSlow(mostPlayerMarchOtherSlow)
                                        .leastPlayerMarchOwnSlow(leastlayerMarchOwnSlow)
                                        .leastPlayerMarchOtherSlow(leastPlayerMarchOtherSlow)
                                        .countMostPlayerMarch(Stream.of(mostPlayerMarchOwnSlow, mostPlayerMarchOtherSlow).flatMap(List::stream).max(Comparator.comparing(EventMarchStatsResponse::getCount)).map(EventMarchStatsResponse::getCount).orElse(0))
                                        .totalNumberMarchs(globalTotalNumberMarchs)
                                        .totalNumberMarchsOwnSlow(globalTotalNumberMarchsOwnSlow)
                                        .totalNumberMarchsOtherSlow(globalTotalNumberMarchsOtherSlow)
                                        .totalNumberMarchsOrdinary(globalTotalNumberMarchsOrdinary)
                                        .totalNumberHours(this.numberService.round(globalTotalNumberHours))
                                        .totalNumberKilometers(this.numberService.round(globalTotalNumberKilometers))
                                        .averageNumberMarchsByHour(this.numberService.round(globalAverageNumberMarchsByHour))
                                        .percentageNumberMarchsOwn(this.numberService.round(globalPercentageNumberMarchsOwn))
                                        .percentageNumberMarchsOther(this.numberService.round(globalPercentageNumberMarchsOther))
                                        .build()
                        )
                        .eventStats(
                                eventStats.stream()
                                        .sorted(Comparator.comparing(event -> event.getEvent().getDate()))
                                        .toList()
                        )
                        .build()
        );
    }

    private Double getEventDuration(final EventResponse eventResponse) {
        if (eventResponse.getDuration() != null) {
            return eventResponse.getDuration();
        }

        // sino hay duracion calculamos a partir del startTime y EndTime (en horas)
        // tenemos que tener en cuenta si la hora final ha pasado de dia...
        if (eventResponse.getEndTime().isBefore(eventResponse.getStartTime())) {
            return (double) (24 - eventResponse.getStartTime().getHour() + eventResponse.getEndTime().getHour());
        }
        return (double) (eventResponse.getEndTime().getHour() - eventResponse.getStartTime().getHour());
    }

    private Integer getNumberMarchCategoryType(final EventRepertoireResponse repertoire, final String category, final String type) {
        return repertoire.getMarchs().stream()
                .flatMap(march -> march.getMarchs().stream())
                .filter(march -> march.getCategory().getCurrent() && march.getCategory().getName().toUpperCase().contains(category.toUpperCase()) && (march.getType().getName().toUpperCase().contains(type.toUpperCase()) || StringUtils.isEmpty(type)))
                .map(RepertoireMarchResponse::getNumbers)
                .reduce(0, Integer::sum);
    }

    private Integer getNumberMarchCategoryNotType(final EventRepertoireResponse repertoire, final String category, final String type) {
        return repertoire.getMarchs().stream()
                .flatMap(march -> march.getMarchs().stream())
                .filter(march -> march.getCategory().getCurrent() && march.getCategory().getName().toUpperCase().contains(category.toUpperCase()) && (!march.getType().getName().toUpperCase().contains(type.toUpperCase()) || StringUtils.isEmpty(type)))
                .map(RepertoireMarchResponse::getNumbers)
                .reduce(0, Integer::sum);
    }

    private Boolean isExclude(final String type, final Boolean excludeSpecialTypes) {
        return (MARCH_TYPE_EXCLUDE_MOST_AND_LEAST.stream().anyMatch(type::contains) && Boolean.TRUE.equals(excludeSpecialTypes));
    }

    private List<EventMarchStatsResponse> getMostPlayerMarchType(final List<EventMarchStatsResponse> marchsStats, final String type, final Boolean excludeSpecialTypes) {

        // obtenemos las marchas mas tocadas, es un array porque pueden ser varias las que se toquen igual numero de veces maximas
        return marchsStats.stream()
                .filter(
                        march -> march.getType().toUpperCase().contains(type.toUpperCase()) &&
                                Boolean.FALSE.equals(this.isExclude(march.getType().toUpperCase(), excludeSpecialTypes))
                )
                .filter(march -> march.getCount().equals(
                        marchsStats.stream()
                                .filter(m -> m.getType().toUpperCase().contains(type.toUpperCase()))
                                .map(EventMarchStatsResponse::getCount)
                                .max(Integer::compareTo)
                                .orElse(0)
                ))
                .toList();

    }

    private List<EventMarchStatsResponse> getLeastPlayerMarchType(final List<EventMarchStatsResponse> marchsStats, final String type, final Boolean excludeSpecialTypes) {

        // obtenemos las marchas menos tocadas, es un array porque pueden ser varias las que se toquen igual numero de veces maximas
        return marchsStats.stream()
                .filter(
                        march -> march.getType().toUpperCase().contains(type.toUpperCase()) &&
                                Boolean.FALSE.equals(this.isExclude(march.getType().toUpperCase(), excludeSpecialTypes))
                )
                .filter(march -> march.getCount().equals(
                        marchsStats.stream()
                                .filter(m -> m.getType().toUpperCase().contains(type.toUpperCase()))
                                .map(EventMarchStatsResponse::getCount)
                                .min(Integer::compareTo)
                                .orElse(0)
                ))
                .toList();
    }

    private List<EventMarchStatsResponse> getMostPlayerMarchNotType(final List<EventMarchStatsResponse> marchsStats, final String type, final Boolean excludeSpecialTypes) {
        // obtenemos las marchas mas tocadas, es un array porque pueden ser varias las que se toquen igual numero de veces maximas
        return marchsStats.stream()
                .filter(
                        march -> !march.getType().toUpperCase().contains(type.toUpperCase()) &&
                                Boolean.FALSE.equals(this.isExclude(march.getType().toUpperCase(), excludeSpecialTypes))
                )
                .filter(march -> march.getCount().equals(
                        marchsStats.stream()
                                .filter(m -> !m.getType().toUpperCase().contains(type.toUpperCase()))
                                .map(EventMarchStatsResponse::getCount)
                                .max(Integer::compareTo)
                                .orElse(0)
                ))
                .toList();
    }

    private List<EventMarchStatsResponse> getLeastPlayerMarchNotType(final List<EventMarchStatsResponse> marchsStats, final String type, final Boolean excludeSpecialTypes) {
        // obtenemos las marchas menos tocadas, es un array porque pueden ser varias las que se toquen igual numero de veces maximas
        return marchsStats.stream()
                .filter(
                        march -> !march.getType().toUpperCase().contains(type.toUpperCase()) &&
                                Boolean.FALSE.equals(this.isExclude(march.getType().toUpperCase(), excludeSpecialTypes))
                )
                .filter(march -> march.getCount().equals(
                        marchsStats.stream()
                                .filter(m -> !m.getType().toUpperCase().contains(type.toUpperCase()))
                                .map(EventMarchStatsResponse::getCount)
                                .min(Integer::compareTo)
                                .orElse(0)
                ))
                .toList();
    }

}
