package com.feyconsuelo.domain.model.repertoireevent;

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
public class RepertoireEventRequest {

    private Long marchId;
    private Long eventId;
    private EventTypeEnum eventType;
}
