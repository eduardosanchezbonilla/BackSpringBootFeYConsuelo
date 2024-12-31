package com.feyconsuelo.infrastructure.converter.repertoireperformance;

import com.feyconsuelo.domain.model.event.EventClsClassEnum;
import com.feyconsuelo.domain.model.event.EventPerformanceTypeEnum;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.infrastructure.entities.repertoireperformance.RepertoirePerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoirePerformanceEntityToEventResponseConverter {

    private List<VoiceResponse> getVoiceList(final RepertoirePerformanceEntity repertoirePerformanceEntity) {
        if (CollectionUtils.isEmpty(repertoirePerformanceEntity.getPerformance().getVoiceIdList())) {
            return List.of();
        } else {
            return repertoirePerformanceEntity.getPerformance().getVoiceIdList().stream()
                    .map(
                            id -> VoiceResponse.builder()
                                    .id(Long.valueOf(id))
                                    .build()
                    )
                    .toList();
        }
    }

    public EventResponse convert(final RepertoirePerformanceEntity repertoirePerformanceEntity) {
        return EventResponse.builder()
                .id(repertoirePerformanceEntity.getPerformance().getId())
                .type(EventTypeEnum.PERFORMANCE)
                .date(repertoirePerformanceEntity.getPerformance().getDate())
                .startTime(repertoirePerformanceEntity.getPerformance().getStartTime())
                .endTime(repertoirePerformanceEntity.getPerformance().getEndTime())
                .title(repertoirePerformanceEntity.getPerformance().getTitle())
                .description(repertoirePerformanceEntity.getPerformance().getDescription())
                .performanceType(EventPerformanceTypeEnum.valueOf(repertoirePerformanceEntity.getPerformance().getPerformanceType()))
                .voiceIdList(repertoirePerformanceEntity.getPerformance().getVoiceIdList())
                .voiceList(this.getVoiceList(repertoirePerformanceEntity))
                .location(repertoirePerformanceEntity.getPerformance().getLocation())
                .municipality(repertoirePerformanceEntity.getPerformance().getMunicipality())
                .province(repertoirePerformanceEntity.getPerformance().getProvince())
                .image(repertoirePerformanceEntity.getPerformance().getImage())
                .clsClass(EventClsClassEnum.ACTUACION_DAY_OK)
                .build();

    }
}
