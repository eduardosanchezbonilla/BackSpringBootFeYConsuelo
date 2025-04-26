package com.feyconsuelo.infrastructure.converter.musicianrehearsal;

import com.feyconsuelo.domain.model.event.EventClsClassEnum;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianRehearsalProjectionToEventResponseConverter {

    private List<VoiceResponse> getVoiceList(final MusicianRehearsalProjection rehearsalEntity) {
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

    public EventResponse convert(final MusicianRehearsalProjection rehearsalEntity) {
        return EventResponse.builder()
                .id(rehearsalEntity.getRehearsalId())
                .type(EventTypeEnum.REHEARSAL)
                .date(rehearsalEntity.getDate())
                .startTime(rehearsalEntity.getStartTime())
                .endTime(rehearsalEntity.getEndTime())
                .description(rehearsalEntity.getDescription())
                .voiceIdList(rehearsalEntity.getVoiceIdList())
                .voiceList(this.getVoiceList(rehearsalEntity))
                .location(rehearsalEntity.getLocation())
                .municipality(rehearsalEntity.getMunicipality())
                .province(rehearsalEntity.getProvince())
                .clsClass(rehearsalEntity.getMusicianId() != null ? EventClsClassEnum.ENSAYO_GENERAL_DAY_OK : EventClsClassEnum.ENSAYO_GENERAL_DAY)
                .formationPositionX(rehearsalEntity.getFormationPositionX())
                .formationPositionY(rehearsalEntity.getFormationPositionY())
                .duration(rehearsalEntity.getDuration())
                .musicianBus(Boolean.FALSE)
                .musicianAssist(rehearsalEntity.getMusicianId() != null)
                .build();

    }
}
