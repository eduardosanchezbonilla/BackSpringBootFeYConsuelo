package com.feyconsuelo.infrastructure.converter.rehearsal;

import com.feyconsuelo.domain.model.event.EventClsClassEnum;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RehearsalEntityToEventResponseConverter {

    private List<VoiceResponse> getVoiceList(final RehearsalEntity rehearsalEntity) {
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

    public EventResponse convert(final RehearsalEntity rehearsalEntity) {
        return EventResponse.builder()
                .id(rehearsalEntity.getId())
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
                .clsClass(EventClsClassEnum.ENSAYO_GENERAL_DAY)
                .build();

    }
}
