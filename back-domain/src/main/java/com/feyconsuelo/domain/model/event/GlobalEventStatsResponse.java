package com.feyconsuelo.domain.model.event;

import lombok.Builder;
import lombok.Value;

import java.util.List;

@Value
@Builder
public class GlobalEventStatsResponse {
    EventDetailStatsResponse totalStats;
    List<EventStatsResponse> eventStats;
}
