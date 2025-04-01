package com.feyconsuelo.domain.model.event;

import lombok.Builder;
import lombok.Value;

import java.util.List;

@Value
@Builder
public class EventDetailStatsResponse {
    List<EventMarchStatsResponse> marchsStats;
    List<EventMarchTypeStatsResponse> marchsTypeStats;
    List<EventMarchStatsResponse> mostPlayerMarchOtherSlow;
    List<EventMarchStatsResponse> mostPlayerMarchOwnSlow;
    List<EventMarchStatsResponse> leastPlayerMarchOtherSlow;
    List<EventMarchStatsResponse> leastPlayerMarchOwnSlow;
    Integer countMostPlayerMarch;
    Integer totalNumberMarchs;
    Integer totalNumberMarchsOwnSlow;
    Integer totalNumberMarchsOtherSlow;
    Integer totalNumberMarchsOrdinary;
    Double totalNumberHours;
    Double totalNumberKilometers;
    Double averageNumberMarchsByHour;
    Double percentageNumberMarchsOwn;
    Double percentageNumberMarchsOther;
}
