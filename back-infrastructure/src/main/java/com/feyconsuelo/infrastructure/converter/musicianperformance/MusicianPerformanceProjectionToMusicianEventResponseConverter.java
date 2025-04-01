package com.feyconsuelo.infrastructure.converter.musicianperformance;

import com.feyconsuelo.domain.model.event.EventClsClassEnum;
import com.feyconsuelo.domain.model.event.EventPerformanceTypeEnum;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventResponse;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianPerformanceProjectionToMusicianEventResponseConverter {

    @Value("${default-images.fake-musician}")
    private String defaultFakeMusicianImage;

    private EventResponse convertPerformance(final MusicianPerformanceProjection performanceProjection) {
        return EventResponse.builder()
                .id(performanceProjection.getPerformance().getId())
                .type(EventTypeEnum.PERFORMANCE)
                .date(performanceProjection.getPerformance().getDate())
                .startTime(performanceProjection.getPerformance().getStartTime())
                .endTime(performanceProjection.getPerformance().getEndTime())
                .title(performanceProjection.getPerformance().getTitle())
                .description(performanceProjection.getPerformance().getDescription())
                .performanceType(EventPerformanceTypeEnum.valueOf(performanceProjection.getPerformance().getPerformanceType()))
                .voiceIdList(performanceProjection.getPerformance().getVoiceIdList())
                .voiceList(List.of())
                .location(performanceProjection.getPerformance().getLocation())
                .municipality(performanceProjection.getPerformance().getMunicipality())
                .province(performanceProjection.getPerformance().getProvince())
                .image(performanceProjection.getPerformance().getImage())
                .clsClass(EventClsClassEnum.ACTUACION_DAY_OK)
                .musicianBus(Boolean.FALSE)
                .formationPositionX(performanceProjection.getFormationPositionX())
                .formationPositionY(performanceProjection.getFormationPositionY())
                .busTime(performanceProjection.getPerformance().getBusTime())
                .busLocation(performanceProjection.getPerformance().getBusLocation())
                .busData(performanceProjection.getPerformance().getBusData())
                .duration(performanceProjection.getPerformance().getDuration())
                .kilometers(performanceProjection.getPerformance().getKilometers())
                .googleId(performanceProjection.getPerformance().getGoogleId())
                .build();

    }

    public MusicianEventResponse convert(final MusicianPerformanceProjection performanceProjection) {
        final EventResponse event = this.convertPerformance(performanceProjection);

        return MusicianEventResponse.builder()
                .musicianResponse(
                        MusicianResponse.builder()
                                .id(performanceProjection.getMusicianId())
                                .dni("")
                                .name("Hueco")
                                .surname("")
                                .direction("")
                                .municipality("")
                                .province("")
                                .email("")
                                .voice(
                                        VoiceResponse.builder()
                                                .order(0)
                                                .name("HUECO")
                                                .build()
                                )
                                .image(this.defaultFakeMusicianImage)
                                .deleteDate(null)
                                .birthDate(null)
                                .registrationDate(null)
                                .unregistrationDate(null)
                                .unregistred(null)
                                .dateLastNotificationNonAssistsStreakRehearsals(null)
                                .inventoryObservations(null)
                                .phoneNumber(null)
                                .formationPositionX(performanceProjection.getFormationPositionX())
                                .formationPositionY(performanceProjection.getFormationPositionY())
                                .assistLastRehearsal(Boolean.TRUE)
                                .observations(null)
                                .build()
                )
                .eventResponse(event)
                .build();
    }
}
