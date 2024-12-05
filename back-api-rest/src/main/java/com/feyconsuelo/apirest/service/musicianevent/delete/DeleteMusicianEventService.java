package com.feyconsuelo.apirest.service.musicianevent.delete;

import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.musicianevent.DeleteMusicianEvent;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteMusicianEventService {

    private final DeleteMusicianEvent deleteMusicianEvent;

    public ResponseEntity<Void> deleteMusicianEvent(final Long musicianId, final EventTypeEnum type, final Long eventId) {
        this.deleteMusicianEvent.execute(musicianId, type, eventId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
