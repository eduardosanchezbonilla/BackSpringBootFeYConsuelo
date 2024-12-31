package com.feyconsuelo.domain.model.event;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchGroupByTypeResponse;
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
public class EventRepertoireResponse {
    private EventResponse event;
    private List<RepertoireMarchGroupByTypeResponse> marchs;
}
