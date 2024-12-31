package com.feyconsuelo.domain.model.musicianevent;

import com.feyconsuelo.domain.model.event.EventTypeEnum;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class MusicianEventRequest {

    private Long musicianId;
    private Long eventId;
    private EventTypeEnum eventType;
    private Boolean bus;
}
