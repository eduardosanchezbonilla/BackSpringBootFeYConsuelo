package com.feyconsuelo.domain.model.event;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class EventStatInfoResponse {

    private Long id;
    private EventTypeEnum type;
    private String title;
    private EventPerformanceTypeEnum performanceType;
    private String description;
    private LocalDate date;
    private LocalDateTime endTime;
    private LocalDateTime startTime;
    private String province;
    private String municipality;
    private String image;
    private Double duration;
    private Double kilometers;
}
