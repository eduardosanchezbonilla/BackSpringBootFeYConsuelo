package com.feyconsuelo.apirest.service.musicianevent.query;

import com.feyconsuelo.apirest.converter.event.EventResponseListToEventResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.event.EventResponseToEventResponseDtoConverter;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.musicianevent.GetAllMusicianEvents;
import com.feyconsuelo.openapi.model.EventResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.time.LocalDate;
import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetMusicianEventService {

    private final GetAllMusicianEvents getAllMusicianEvents;

    private final EventResponseToEventResponseDtoConverter eventResponseToEventResponseDtoConverter;

    private final EventResponseListToEventResponseDtoListConverter eventResponseListToEventResponseDtoListConverter;

    public ResponseEntity<List<EventResponseDto>> getAllMusicianEvents(final Long musicianId, final EventTypeEnum eventType, final LocalDate startDate, final LocalDate endDate) {
        final List<EventResponse> eventResponseList = this.getAllMusicianEvents.execute(musicianId, startDate, endDate, eventType);
        if (CollectionUtils.isEmpty(eventResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.eventResponseListToEventResponseDtoListConverter.convert(eventResponseList));
    }


}
