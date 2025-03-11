package com.feyconsuelo.domain.model.event;

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
public class EventCrossheadMarch {

    private Long id;
    private Long marchId;
    private String marchName;
    private Integer marchOrder;
    private String annotations;

}
