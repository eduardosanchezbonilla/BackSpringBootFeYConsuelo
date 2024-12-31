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
public class EventRequest {

    private EventTypeEnum type;
    private String title;
    private String description;
    private EventPerformanceTypeEnum performanceType;
    private LocalDate date;
    private LocalDateTime startTime;
    private LocalDateTime endTime;
    private List<Integer> voiceIdList;
    private List<VoiceResponse> voiceList;
    private EventRepetitionPeriodEnum repetitionPeriod;
    private LocalDate endDate;
    private String location;
    private String municipality;
    private String province;
    private String image;
    private Boolean displacementBus;

}
