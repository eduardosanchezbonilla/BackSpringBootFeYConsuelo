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

    @SuppressWarnings("java:S3776")
    private EventResponse convertPerformance(final MusicianPerformanceProjection performanceProjection) {
        return EventResponse.builder()
                .id(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getId() : performanceProjection.getPerformanceId())
                .type(EventTypeEnum.PERFORMANCE)
                .date(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getDate() : performanceProjection.getDate())
                .startTime(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getStartTime() : performanceProjection.getStartTime())
                .endTime(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getEndTime() : performanceProjection.getEndTime())
                .title(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getTitle() : performanceProjection.getTitle())
                .description(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getDescription() : performanceProjection.getDescription())
                .performanceType(performanceProjection.getPerformance() != null ? EventPerformanceTypeEnum.valueOf(performanceProjection.getPerformance().getPerformanceType()) : EventPerformanceTypeEnum.valueOf(performanceProjection.getPerformanceType()))
                .voiceIdList(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getVoiceIdList() : performanceProjection.getVoiceIdList())
                .voiceList(List.of())
                .location(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getLocation() : performanceProjection.getLocation())
                .municipality(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getMunicipality() : performanceProjection.getMunicipality())
                .province(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getProvince() : performanceProjection.getProvince())
                .image(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getImage() : performanceProjection.getImageThumbnail())
                .displacementBus(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getBus() : performanceProjection.getDisplacementBus())
                .eventPublic(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getEventPublic() : performanceProjection.getEventPublic())
                .repertoirePublic(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getRepertoirePublic() : performanceProjection.getRepertoirePublic())
                .crossheadPublic(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getCrossheadPublic() : performanceProjection.getCrossheadPublic())
                .busTime(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getBusTime() : performanceProjection.getBusTime())
                .busLocation(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getBusLocation() : performanceProjection.getBusLocation())
                .busData(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getBusData() : performanceProjection.getBusData())
                .duration(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getDuration() : performanceProjection.getDuration())
                .kilometers(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getKilometers() : performanceProjection.getKilometers())
                .googleId(performanceProjection.getPerformance() != null ? performanceProjection.getPerformance().getGoogleId() : performanceProjection.getGoogleId())
                .clsClass(EventClsClassEnum.ACTUACION_DAY_OK)
                .musicianBus(performanceProjection.getPerformance() != null ? Boolean.FALSE : performanceProjection.getMusicianBus())
                .formationPositionX(performanceProjection.getFormationPositionX())
                .formationPositionY(performanceProjection.getFormationPositionY())
                .build();

    }

    private MusicianResponse convertMusicianFake(final MusicianPerformanceProjection performanceProjection) {
        return MusicianResponse.builder()
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
                .build();
    }

    private MusicianResponse convertMusicianNotFake(final MusicianPerformanceProjection performanceProjection) {
        return MusicianResponse.builder()
                .id(performanceProjection.getMusicianId())
                .dni(performanceProjection.getMusicianDni())
                .name(performanceProjection.getMusicianName())
                .surname(performanceProjection.getMusicianSurname())
                .direction(performanceProjection.getMusicianDirection())
                .municipality(performanceProjection.getMusicianMunicipality())
                .province(performanceProjection.getMusicianProvince())
                .email(performanceProjection.getMusicianEmail())
                .voice(
                        VoiceResponse.builder()
                                .id(performanceProjection.getVoiceId())
                                .order(performanceProjection.getVoiceOrder())
                                .name(performanceProjection.getVoiceName())
                                .build()
                )
                .image(performanceProjection.getMusicianImage())
                .deleteDate(performanceProjection.getMusicianDeleteDate())
                .birthDate(performanceProjection.getMusicianBirthDate())
                .registrationDate(performanceProjection.getMusicianRegistrationDate())
                .unregistrationDate(performanceProjection.getMusicianUnregistrationDate())
                .unregistred(performanceProjection.getMusicianUnregistred())
                .dateLastNotificationNonAssistsStreakRehearsals(performanceProjection.getMusicianDateLastNotificationNonAssistsStreakRehearsals())
                .inventoryObservations(performanceProjection.getMusicianInventoryObservations())
                .phoneNumber(performanceProjection.getMusicianPhoneNumber())
                .formationPositionX(performanceProjection.getFormationPositionX())
                .formationPositionY(performanceProjection.getFormationPositionY())
                .assistLastRehearsal(Boolean.TRUE)
                .observations(performanceProjection.getMusicianObservations())
                .build();
    }

    public MusicianEventResponse convert(final MusicianPerformanceProjection performanceProjection, final Boolean musicianFake) {
        final EventResponse event = this.convertPerformance(performanceProjection);

        return MusicianEventResponse.builder()
                .musicianResponse(
                        Boolean.TRUE.equals(musicianFake) ?
                                this.convertMusicianFake(performanceProjection) :
                                this.convertMusicianNotFake(performanceProjection)
                )
                .eventResponse(event)
                .build();
    }
}
