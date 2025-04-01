package com.feyconsuelo.domain.model.event;

import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class EventMarchTypeStatsResponse {

    private String name;
    private Integer count;
    private String image;
    private Integer orderType;
    private List<EventMarchStatsResponse> mostPlayerMarch;
    private List<EventMarchStatsResponse> leastPlayerMarch;
}
