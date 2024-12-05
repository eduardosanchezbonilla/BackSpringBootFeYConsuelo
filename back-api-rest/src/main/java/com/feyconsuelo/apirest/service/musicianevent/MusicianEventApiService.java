package com.feyconsuelo.apirest.service.musicianevent;

import com.feyconsuelo.apirest.service.musicianevent.delete.DeleteMusicianEventService;
import com.feyconsuelo.apirest.service.musicianevent.insert.InsertMusicianEventService;
import com.feyconsuelo.apirest.service.musicianevent.query.GetMusicianEventService;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.openapi.api.MusicianEventControllerApiDelegate;
import com.feyconsuelo.openapi.model.EventResponseDto;
import com.feyconsuelo.openapi.model.MusicianEventRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class MusicianEventApiService implements MusicianEventControllerApiDelegate {

    private final InsertMusicianEventService insertMusicianEventService;
    private final DeleteMusicianEventService deleteMusicianEventService;
    private final GetMusicianEventService getMusicianEventService;

    @Override
    public ResponseEntity<Void> deleteMusicianEvent(final Long musicianId,
                                                    final String eventType,
                                                    final Long eventId) {
        return this.deleteMusicianEventService.deleteMusicianEvent(
                musicianId,
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId
        );
    }

    @Override
    public ResponseEntity<Void> postMusicianEvent(final Long musicianId,
                                                  final String eventType,
                                                  final Long eventId,
                                                  final MusicianEventRequestDto musicianEventRequestDto) {
        return this.insertMusicianEventService.postMusicianEvent(
                musicianId,
                eventType,
                eventId,
                musicianEventRequestDto
        );
    }

    @Override
    public ResponseEntity<List<EventResponseDto>> getAllMusicianEvents(final Long musicianId,
                                                                       final String eventType,
                                                                       final LocalDate startDate,
                                                                       final LocalDate endDate) {
        return this.getMusicianEventService.getAllMusicianEvents(
                musicianId,
                StringUtils.isEmpty(eventType) ? null : EventTypeEnum.valueOf(eventType.toUpperCase()),
                startDate,
                endDate
        );
    }
}
