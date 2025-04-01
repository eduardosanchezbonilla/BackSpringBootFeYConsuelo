package com.feyconsuelo.domain.model.event;

import lombok.Builder;
import lombok.Value;

@Value
@Builder
public class EventStatsResponse {
    EventStatInfoResponse event;
    EventDetailStatsResponse stats;
}
