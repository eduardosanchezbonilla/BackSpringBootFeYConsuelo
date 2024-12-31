package com.feyconsuelo.domain.model.repertoireevent;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
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
public class RepertoireEventResponse {
    private RepertoireMarchResponse repertoireMarchResponse;
    private EventResponse eventResponse;
}
