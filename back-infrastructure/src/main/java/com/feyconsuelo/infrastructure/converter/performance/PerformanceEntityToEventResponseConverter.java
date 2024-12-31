package com.feyconsuelo.infrastructure.converter.performance;

import com.feyconsuelo.domain.model.event.EventClsClassEnum;
import com.feyconsuelo.domain.model.event.EventPerformanceTypeEnum;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.infrastructure.entities.performance.PerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class PerformanceEntityToEventResponseConverter {

    private List<VoiceResponse> getVoiceList(final PerformanceEntity performanceEntity) {
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

    public EventResponse convert(final PerformanceEntity performanceEntity) {
        return EventResponse.builder()
                .id(performanceEntity.getId())
                .type(EventTypeEnum.PERFORMANCE)
                .date(performanceEntity.getDate())
                .startTime(performanceEntity.getStartTime())
                .endTime(performanceEntity.getEndTime())
                .title(performanceEntity.getTitle())
                .description(performanceEntity.getDescription())
                .performanceType(EventPerformanceTypeEnum.valueOf(performanceEntity.getPerformanceType()))
                .voiceIdList(performanceEntity.getVoiceIdList())
                .voiceList(this.getVoiceList(performanceEntity))
                .location(performanceEntity.getLocation())
                .municipality(performanceEntity.getMunicipality())
                .province(performanceEntity.getProvince())
                .image(performanceEntity.getImage())
                .clsClass(EventClsClassEnum.ACTUACION_DAY)
                .displacementBus(performanceEntity.getBus())
                .build();

    }
}
