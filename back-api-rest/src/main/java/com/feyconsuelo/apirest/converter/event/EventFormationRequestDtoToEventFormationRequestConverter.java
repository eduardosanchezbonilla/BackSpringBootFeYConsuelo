package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.EventFormationRequest;
import com.feyconsuelo.domain.model.musician.MusicianFormationRequest;
import com.feyconsuelo.openapi.model.EventFormationRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventFormationRequestDtoToEventFormationRequestConverter {

    public EventFormationRequest convert(final EventFormationRequestDto eventFormationRequestDto) {
        return EventFormationRequest.builder()
                .musicians(
                        CollectionUtils.isEmpty(eventFormationRequestDto.getMusicians()) ?
                                List.of() :
                                eventFormationRequestDto.getMusicians().stream()
                                        .map(musician -> MusicianFormationRequest.builder()
                                                .musicianId(musician.getId())
                                                .formationPositionX(musician.getFormationPositionX())
                                                .formationPositionY(musician.getFormationPositionY())
                                                .build()
                                        )
                                        .toList()
                )
                .build();
    }

}
