package com.feyconsuelo.domain.model.event;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class EventCrossheadStreet {

    private Long id;
    private String street;
    private Integer streetOrder;
    private String annotations;
    private List<EventCrossheadMarch> marchs;

}
