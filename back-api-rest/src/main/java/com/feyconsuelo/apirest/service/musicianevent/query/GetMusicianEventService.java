package com.feyconsuelo.apirest.service.musicianevent.query;

import com.feyconsuelo.apirest.converter.musicianevent.MusicianEventListResponseToMusicianEventListResponseDtoConverter;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventListResponse;
import com.feyconsuelo.domain.usecase.musicianevent.GetAllMusicianEvents;
import com.feyconsuelo.openapi.model.MusicianEventListResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.time.LocalDate;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetMusicianEventService {

    private final GetAllMusicianEvents getAllMusicianEvents;

    private final MusicianEventListResponseToMusicianEventListResponseDtoConverter musicianEventListResponseToMusicianEventListResponseDtoConverter;

    public ResponseEntity<MusicianEventListResponseDto> getAllMusicianEvents(final Long musicianId, final EventTypeEnum eventType, final LocalDate startDate, final LocalDate endDate) {
        final MusicianEventListResponse musicianEventListResponse = this.getAllMusicianEvents.execute(musicianId, startDate, endDate, eventType);
        return ResponseEntity.ok(this.musicianEventListResponseToMusicianEventListResponseDtoConverter.convert(musicianEventListResponse, Boolean.TRUE));
    }


}
