package com.feyconsuelo.domain.model.event;

import com.feyconsuelo.domain.model.voice.VoiceResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class EventResponse {

    private Long id;
    private EventTypeEnum type;
    private String title;
    private EventPerformanceTypeEnum performanceType;
    private String description;
    private LocalDate date;
    private LocalDateTime endTime;
    private LocalDateTime startTime;
    private List<Integer> voiceIdList;
    private List<VoiceResponse> voiceList;
    private EventRepetitionPeriodEnum repetitionPeriod;
    private LocalDate endDate;
    private String location;
    private String province;
    private String municipality;
    private String image;
    private EventClsClassEnum clsClass;
    private Boolean displacementBus;
    private Boolean musicianBus;
    private Boolean musicianAssist;

}
