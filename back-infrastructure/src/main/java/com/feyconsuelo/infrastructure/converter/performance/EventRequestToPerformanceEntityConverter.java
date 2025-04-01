package com.feyconsuelo.infrastructure.converter.performance;

import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventRouteRequest;
import com.feyconsuelo.domain.model.event.LatLng;
import com.feyconsuelo.infrastructure.entities.performance.PerformanceEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventRequestToPerformanceEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;
    private final EventRouteRequestToJsonStringConverter eventRouteRequestToJsonStringConverter;

    @Value("${default-images.event}")
    private String defaultVoiceEvent;

    private String getPerformanceImage(final String image) {
        if (StringUtils.isEmpty(image)) {
            return null;
        } else {
            if (image.equals(this.defaultVoiceEvent)) {
                return null;
            } else {
                return image;
            }
        }
    }

    public PerformanceEntity convert(final EventRequest eventRequest) {
        return PerformanceEntity.builder()
                .date(eventRequest.getDate())
                .startTime(eventRequest.getStartTime())
                .endTime(eventRequest.getEndTime())
                .title(eventRequest.getTitle())
                .description(eventRequest.getDescription())
                .performanceType(eventRequest.getPerformanceType().getId())
                .voiceIdList(eventRequest.getVoiceIdList())
                .location(eventRequest.getLocation())
                .municipality(eventRequest.getMunicipality())
                .province(eventRequest.getProvince())
                .image(this.getPerformanceImage(eventRequest.getImage()))
                .imageThumbnail(this.getPerformanceImage(eventRequest.getImageThumbnail()))
                .bus(eventRequest.getDisplacementBus())
                .eventPublic(eventRequest.getEventPublic())
                .repertoirePublic(eventRequest.getRepertoirePublic())
                .crossheadPublic(eventRequest.getCrossheadPublic())
                .busData(eventRequest.getBusData())
                .busTime(eventRequest.getBusTime())
                .busLocation(eventRequest.getBusLocation())
                .modifiedUser(this.tokenInfoExtractorService.getUsername())
                .duration(eventRequest.getDuration())
                .kilometers(eventRequest.getKilometers())
                .googleId(eventRequest.getGoogleId())
                .build();
    }

    public PerformanceEntity updateEntity(final PerformanceEntity performanceEntity,
                                          final EventRequest eventRequest) {
        performanceEntity.setDate(eventRequest.getDate());
        performanceEntity.setStartTime(eventRequest.getStartTime());
        performanceEntity.setEndTime(eventRequest.getEndTime());
        performanceEntity.setTitle(eventRequest.getTitle());
        performanceEntity.setDescription(eventRequest.getDescription());
        performanceEntity.setPerformanceType(eventRequest.getPerformanceType().getId());
        performanceEntity.setVoiceIdList(eventRequest.getVoiceIdList());
        performanceEntity.setLocation(eventRequest.getLocation());
        performanceEntity.setMunicipality(eventRequest.getMunicipality());
        performanceEntity.setProvince(eventRequest.getProvince());
        performanceEntity.setImage(this.getPerformanceImage(eventRequest.getImage()));
        performanceEntity.setImageThumbnail(this.getPerformanceImage(eventRequest.getImageThumbnail()));
        performanceEntity.setBus(eventRequest.getDisplacementBus());
        performanceEntity.setEventPublic(eventRequest.getEventPublic());
        performanceEntity.setRepertoirePublic(eventRequest.getRepertoirePublic());
        performanceEntity.setCrossheadPublic(eventRequest.getCrossheadPublic());
        performanceEntity.setBusData(eventRequest.getBusData());
        performanceEntity.setBusTime(eventRequest.getBusTime());
        performanceEntity.setBusLocation(eventRequest.getBusLocation());
        performanceEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());
        performanceEntity.setDuration(eventRequest.getDuration());
        performanceEntity.setKilometers(eventRequest.getKilometers());
        performanceEntity.setGoogleId(eventRequest.getGoogleId());

        return performanceEntity;
    }

    public PerformanceEntity deleteEntity(final PerformanceEntity performanceEntity) {
        performanceEntity.setDeleteDate(LocalDateTime.now());
        performanceEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return performanceEntity;
    }

    public PerformanceEntity updateEntityRoute(final PerformanceEntity performanceEntity,
                                               final EventRouteRequest eventRouteRequest) {
        performanceEntity.setRoute(this.eventRouteRequestToJsonStringConverter.convert(eventRouteRequest));
        performanceEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());
        performanceEntity.setKilometers(eventRouteRequest.getKilometers());

        return performanceEntity;
    }

    public PerformanceEntity updateEntityCurrentPosition(final PerformanceEntity performanceEntity,
                                                         final LatLng latLng) {
        performanceEntity.setCurrentLat(latLng.getLat());
        performanceEntity.setCurrentLng(latLng.getLng());
        performanceEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return performanceEntity;
    }

    public PerformanceEntity updateEntityCurrentMarch(final PerformanceEntity performanceEntity,
                                                      final String march) {
        performanceEntity.setCurrentMarch(march);
        performanceEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return performanceEntity;
    }
}
