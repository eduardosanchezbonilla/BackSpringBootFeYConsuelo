package com.feyconsuelo.infrastructure.converter.musicianrehearsal;

import com.feyconsuelo.domain.model.event.EventClsClassEnum;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianRehearsalEntityToEventResponseConverter {

    private List<VoiceResponse> getVoiceList(final MusicianRehearsalEntity rehearsalEntity) {
        if (CollectionUtils.isEmpty(rehearsalEntity.getRehearsal().getVoiceIdList())) {
            return List.of();
        } else {
            return rehearsalEntity.getRehearsal().getVoiceIdList().stream()
                    .map(
                            id -> VoiceResponse.builder()
                                    .id(Long.valueOf(id))
                                    .build()
                    )
                    .toList();
        }
    }

    public EventResponse convert(final MusicianRehearsalEntity rehearsalEntity) {
        return EventResponse.builder()
                .id(rehearsalEntity.getRehearsal().getId())
                .type(EventTypeEnum.REHEARSAL)
                .date(rehearsalEntity.getRehearsal().getDate())
                .startTime(rehearsalEntity.getRehearsal().getStartTime())
                .endTime(rehearsalEntity.getRehearsal().getEndTime())
                .description(rehearsalEntity.getRehearsal().getDescription())
                .voiceIdList(rehearsalEntity.getRehearsal().getVoiceIdList())
                .voiceList(this.getVoiceList(rehearsalEntity))
                .location(rehearsalEntity.getRehearsal().getLocation())
                .municipality(rehearsalEntity.getRehearsal().getMunicipality())
                .province(rehearsalEntity.getRehearsal().getProvince())
                .clsClass(EventClsClassEnum.ENSAYO_GENERAL_DAY_OK)
                .formationPositionX(rehearsalEntity.getFormationPositionX())
                .formationPositionY(rehearsalEntity.getFormationPositionY())
                .build();

    }
}
