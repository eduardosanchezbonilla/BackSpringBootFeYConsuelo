package com.feyconsuelo.domain.model.event;

import com.feyconsuelo.domain.model.musician.MusicianFormationRequest;
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
public class EventFormationRequest {

    private List<MusicianFormationRequest> musicians;

}
