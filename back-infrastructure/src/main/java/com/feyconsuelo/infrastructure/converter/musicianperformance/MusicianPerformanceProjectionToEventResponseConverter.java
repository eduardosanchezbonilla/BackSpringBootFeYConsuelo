package com.feyconsuelo.infrastructure.converter.musicianperformance;

import com.feyconsuelo.domain.model.event.EventClsClassEnum;
import com.feyconsuelo.domain.model.event.EventPerformanceTypeEnum;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.event.LatLng;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.infrastructure.converter.performance.EventRouteStringToEventRouteResponseConverter;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianPerformanceProjectionToEventResponseConverter {

    private final EventRouteStringToEventRouteResponseConverter eventRouteStringToEventRouteResponseConverter;

    private List<VoiceResponse> getVoiceList(final MusicianPerformanceProjection rehearsalEntity) {
        if (CollectionUtils.isEmpty(rehearsalEntity.getVoiceIdList())) {
            return List.of();
        } else {
            return rehearsalEntity.getVoiceIdList().stream()
                    .map(
                            id -> VoiceResponse.builder()
                                    .id(Long.valueOf(id))
                                    .build()
                    )
                    .toList();
        }
    }

    public EventResponse convert(final MusicianPerformanceProjection performanceProjection) {
        return EventResponse.builder()
                .id(performanceProjection.getPerformanceId())
                .type(EventTypeEnum.PERFORMANCE)
                .date(performanceProjection.getDate())
                .startTime(performanceProjection.getStartTime())
                .endTime(performanceProjection.getEndTime())
                .title(performanceProjection.getTitle())
                .description(performanceProjection.getDescription())
                .performanceType(EventPerformanceTypeEnum.valueOf(performanceProjection.getPerformanceType()))
                .voiceIdList(performanceProjection.getVoiceIdList())
                .voiceList(this.getVoiceList(performanceProjection))
                .location(performanceProjection.getLocation())
                .municipality(performanceProjection.getMunicipality())
                .province(performanceProjection.getProvince())
                .image(performanceProjection.getImageThumbnail())
                .clsClass(performanceProjection.getMusicianId() != null ? EventClsClassEnum.ACTUACION_DAY_OK : EventClsClassEnum.ACTUACION_DAY)
                .displacementBus(performanceProjection.getDisplacementBus())
                .eventPublic(performanceProjection.getEventPublic())
                .repertoirePublic(performanceProjection.getRepertoirePublic())
                .crossheadPublic(performanceProjection.getCrossheadPublic())
                .busData(performanceProjection.getBusData())
                .busTime(performanceProjection.getBusTime())
                .busLocation(performanceProjection.getBusLocation())
                .route(this.eventRouteStringToEventRouteResponseConverter.convert(performanceProjection.getRoute()))
                .currentPosition(
                        LatLng.builder()
                                .lat(performanceProjection.getCurrentLatitude())
                                .lng(performanceProjection.getCurrentLongitude())
                                .build()
                )
                .currentMarch(performanceProjection.getCurrentMarch())
                .duration(performanceProjection.getDuration())
                .kilometers(performanceProjection.getKilometers())
                .googleId(performanceProjection.getGoogleId())
                .musicianBus(performanceProjection.getMusicianBus())
                .formationPositionX(performanceProjection.getFormationPositionX())
                .formationPositionY(performanceProjection.getFormationPositionY())
                .duration(performanceProjection.getDuration())
                .musicianAssist(performanceProjection.getMusicianId() != null)
                .build();
    }

}
