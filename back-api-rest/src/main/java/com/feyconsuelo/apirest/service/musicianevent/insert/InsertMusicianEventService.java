package com.feyconsuelo.apirest.service.musicianevent.insert;

import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventRequest;
import com.feyconsuelo.domain.usecase.musicianevent.InsertMusicianEvent;
import com.feyconsuelo.openapi.model.MusicianEventRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertMusicianEventService {

    private final InsertMusicianEvent insertMusicianEvent;

    public ResponseEntity<Void> postMusicianEvent(final Long musicianId,
                                                  final String eventType,
                                                  final Long eventId,
                                                  final MusicianEventRequestDto musicianEventRequestDto) {
        this.insertMusicianEvent.execute(
                MusicianEventRequest.builder()
                        .musicianId(musicianId)
                        .eventId(eventId)
                        .eventType(EventTypeEnum.valueOf(eventType.toUpperCase()))
                        .bus(musicianEventRequestDto.getBus())
                        .build()
        );
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }
    
}
