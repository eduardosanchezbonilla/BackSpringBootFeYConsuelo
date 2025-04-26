package com.feyconsuelo.infrastructure.converter.performance;

import com.feyconsuelo.domain.model.event.EventClsClassEnum;
import com.feyconsuelo.domain.model.event.EventMusiciansResponse;
import com.feyconsuelo.domain.model.event.EventPerformanceTypeEnum;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.event.LatLng;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.infrastructure.entities.performance.PerformanceMusiciansProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.stream.Stream;

@Slf4j
@Component
@RequiredArgsConstructor
public class PerformanceMusiciansProjectionToEventMusiciansResponseConverter {

    private final EventRouteStringToEventRouteResponseConverter eventRouteStringToEventRouteResponseConverter;
    private final MusiciansStringToMusicianResponseListConverter musiciansStringToMusicianResponseListConverter;
    @Value("${default-images.fake-musician}")
    private String defaultFakeMusicianImage;

    private List<VoiceResponse> getVoiceList(final PerformanceMusiciansProjection performanceEntity) {
        if (CollectionUtils.isEmpty(performanceEntity.getVoiceIdList())) {
            return List.of();
        } else {
            return performanceEntity.getVoiceIdList().stream()
                    .map(
                            id -> VoiceResponse.builder()
                                    .id(Long.valueOf(id))
                                    .build()
                    )
                    .toList();
        }
    }

    public EventMusiciansResponse convert(final PerformanceMusiciansProjection performanceMusiciansProjection, final Boolean route) {

        final List<MusicianResponse> musicians = this.musiciansStringToMusicianResponseListConverter.convert(performanceMusiciansProjection.getMusicians());
        final List<MusicianResponse> fakeMusicians = this.musiciansStringToMusicianResponseListConverter.convert(performanceMusiciansProjection.getFakeMusicians());

        // a todos los fake popngo la foto del hueco
        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(fakeMusicians))) {
            fakeMusicians.forEach(musician -> musician.setImage(this.defaultFakeMusicianImage));
        }

        return EventMusiciansResponse.builder()
                .event(
                        EventResponse.builder()
                                .id(performanceMusiciansProjection.getPerformanceId())
                                .type(EventTypeEnum.PERFORMANCE)
                                .date(performanceMusiciansProjection.getDate())
                                .startTime(performanceMusiciansProjection.getStartTime())
                                .endTime(performanceMusiciansProjection.getEndTime())
                                .title(performanceMusiciansProjection.getTitle())
                                .description(performanceMusiciansProjection.getDescription())
                                .performanceType(EventPerformanceTypeEnum.valueOf(performanceMusiciansProjection.getPerformanceType()))
                                .voiceIdList(performanceMusiciansProjection.getVoiceIdList())
                                .voiceList(this.getVoiceList(performanceMusiciansProjection))
                                .location(performanceMusiciansProjection.getLocation())
                                .municipality(performanceMusiciansProjection.getMunicipality())
                                .province(performanceMusiciansProjection.getProvince())
                                .image(performanceMusiciansProjection.getImageThumbnail())
                                .clsClass(EventClsClassEnum.ACTUACION_DAY)
                                .displacementBus(performanceMusiciansProjection.getDisplacementBus())
                                .eventPublic(performanceMusiciansProjection.getEventPublic())
                                .repertoirePublic(performanceMusiciansProjection.getRepertoirePublic())
                                .crossheadPublic(performanceMusiciansProjection.getCrossheadPublic())
                                .busData(performanceMusiciansProjection.getBusData())
                                .busTime(performanceMusiciansProjection.getBusTime())
                                .busLocation(performanceMusiciansProjection.getBusLocation())
                                .route(Boolean.TRUE.equals(route) ? this.eventRouteStringToEventRouteResponseConverter.convert(performanceMusiciansProjection.getRoute()) : null)
                                .currentPosition(
                                        LatLng.builder()
                                                .lat(performanceMusiciansProjection.getCurrentLatitude())
                                                .lng(performanceMusiciansProjection.getCurrentLongitude())
                                                .build()
                                )
                                .currentMarch(performanceMusiciansProjection.getCurrentMarch())
                                .duration(performanceMusiciansProjection.getDuration())
                                .kilometers(performanceMusiciansProjection.getKilometers())
                                .googleId(performanceMusiciansProjection.getGoogleId())
                                .build()
                )
                .musicians(Stream.concat(musicians.stream(), fakeMusicians.stream()).toList())
                .build();
    }
}
