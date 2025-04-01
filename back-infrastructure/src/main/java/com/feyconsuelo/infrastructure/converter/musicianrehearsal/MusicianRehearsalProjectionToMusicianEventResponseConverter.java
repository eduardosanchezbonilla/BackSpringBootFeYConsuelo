package com.feyconsuelo.infrastructure.converter.musicianrehearsal;

import com.feyconsuelo.domain.model.event.EventClsClassEnum;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventResponse;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianRehearsalProjectionToMusicianEventResponseConverter {

    @Value("${default-images.fake-musician}")
    private String defaultFakeMusicianImage;

    private EventResponse convertRehearsal(final MusicianRehearsalProjection rehearsalProjection) {
        return EventResponse.builder()
                .id(rehearsalProjection.getRehearsal().getId())
                .type(EventTypeEnum.REHEARSAL)
                .date(rehearsalProjection.getRehearsal().getDate())
                .startTime(rehearsalProjection.getRehearsal().getStartTime())
                .endTime(rehearsalProjection.getRehearsal().getEndTime())
                .description(rehearsalProjection.getRehearsal().getDescription())
                .voiceIdList(rehearsalProjection.getRehearsal().getVoiceIdList())
                .voiceList(List.of())
                .location(rehearsalProjection.getRehearsal().getLocation())
                .municipality(rehearsalProjection.getRehearsal().getMunicipality())
                .province(rehearsalProjection.getRehearsal().getProvince())
                .clsClass(EventClsClassEnum.ENSAYO_GENERAL_DAY_OK)
                .formationPositionX(rehearsalProjection.getFormationPositionX())
                .formationPositionY(rehearsalProjection.getFormationPositionY())
                .duration(rehearsalProjection.getRehearsal().getDuration())
                .build();

    }

    public MusicianEventResponse convert(final MusicianRehearsalProjection musicianRehearsalProjection) {
        final EventResponse event = this.convertRehearsal(musicianRehearsalProjection);

        return MusicianEventResponse.builder()
                .musicianResponse(
                        MusicianResponse.builder()
                                .id(musicianRehearsalProjection.getMusicianId())
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
                                .formationPositionX(musicianRehearsalProjection.getFormationPositionX())
                                .formationPositionY(musicianRehearsalProjection.getFormationPositionY())
                                .assistLastRehearsal(Boolean.TRUE)
                                .observations(null)
                                .build()
                )
                .eventResponse(event)
                .build();
    }
}
