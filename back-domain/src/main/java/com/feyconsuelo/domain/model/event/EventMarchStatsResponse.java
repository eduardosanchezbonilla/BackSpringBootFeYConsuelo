package com.feyconsuelo.domain.model.event;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class EventMarchStatsResponse {

    private String name;
    private Integer count;
    private Integer orderType;
    private String type;
    private String category;
    private String image;
}
