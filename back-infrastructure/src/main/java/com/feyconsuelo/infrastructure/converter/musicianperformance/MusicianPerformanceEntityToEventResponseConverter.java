package com.feyconsuelo.infrastructure.converter.musicianperformance;

import com.feyconsuelo.domain.model.event.EventClsClassEnum;
import com.feyconsuelo.domain.model.event.EventPerformanceTypeEnum;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianPerformanceEntityToEventResponseConverter {

    private List<VoiceResponse> getVoiceList(final MusicianPerformanceEntity musicianPerformanceEntity) {
        if (CollectionUtils.isEmpty(musicianPerformanceEntity.getPerformance().getVoiceIdList())) {
            return List.of();
        } else {
            return musicianPerformanceEntity.getPerformance().getVoiceIdList().stream()
                    .map(
                            id -> VoiceResponse.builder()
                                    .id(Long.valueOf(id))
                                    .build()
                    )
                    .toList();
        }
    }

    public EventResponse convert(final MusicianPerformanceEntity musicianPerformanceEntity) {
        return EventResponse.builder()
                .id(musicianPerformanceEntity.getPerformance().getId())
                .type(EventTypeEnum.PERFORMANCE)
                .date(musicianPerformanceEntity.getPerformance().getDate())
                .startTime(musicianPerformanceEntity.getPerformance().getStartTime())
                .endTime(musicianPerformanceEntity.getPerformance().getEndTime())
                .title(musicianPerformanceEntity.getPerformance().getTitle())
                .description(musicianPerformanceEntity.getPerformance().getDescription())
                .performanceType(EventPerformanceTypeEnum.valueOf(musicianPerformanceEntity.getPerformance().getPerformanceType()))
                .voiceIdList(musicianPerformanceEntity.getPerformance().getVoiceIdList())
                .voiceList(this.getVoiceList(musicianPerformanceEntity))
                .location(musicianPerformanceEntity.getPerformance().getLocation())
                .municipality(musicianPerformanceEntity.getPerformance().getMunicipality())
                .province(musicianPerformanceEntity.getPerformance().getProvince())
                .image(musicianPerformanceEntity.getPerformance().getImage())
                .clsClass(EventClsClassEnum.ACTUACION_DAY_OK)
                .musicianBus(musicianPerformanceEntity.getBus())
                .formationPositionX(musicianPerformanceEntity.getFormationPositionX())
                .formationPositionY(musicianPerformanceEntity.getFormationPositionY())
                .build();

    }
}
