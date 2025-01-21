package com.feyconsuelo.apirest.converter.musicianevent;

import com.feyconsuelo.apirest.converter.event.EventResponseListToEventResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.statistics.EventAssistStatisticsResponseToEventAssistStatisticsResponseDtoConverter;
import com.feyconsuelo.apirest.converter.statistics.MusicianAssistInformationResponseToMusicianAssistInformationResponseResponseDtoConverter;
import com.feyconsuelo.apirest.converter.statistics.MusicianEventAssistStatisticsResponseToMusicianEventAssistStatisticsResponseDtoConverter;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventListResponse;
import com.feyconsuelo.openapi.model.MusicianEventListResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianEventListResponseToMusicianEventListResponseDtoConverter {

    private final EventResponseListToEventResponseDtoListConverter eventResponseListToEventResponseDtoListConverter;
    private final MusicianEventAssistStatisticsResponseToMusicianEventAssistStatisticsResponseDtoConverter musicianEventAssistStatisticsResponseToMusicianEventAssistStatisticsResponseDtoConverter;
    private final EventAssistStatisticsResponseToEventAssistStatisticsResponseDtoConverter eventAssistStatisticsResponseToEventAssistStatisticsResponseDtoConverter;
    private final MusicianAssistInformationResponseToMusicianAssistInformationResponseResponseDtoConverter musicianAssistInformationResponseToMusicianAssistInformationResponseResponseDtoConverter;

    public MusicianEventListResponseDto convert(final MusicianEventListResponse musicianEventListResponse) {
        return MusicianEventListResponseDto.builder()
                .eventAssistStatistic(this.eventAssistStatisticsResponseToEventAssistStatisticsResponseDtoConverter.convert(musicianEventListResponse.getEventAssistStatisticsResponse()))
                .musicianEventAssistStatistic(this.musicianEventAssistStatisticsResponseToMusicianEventAssistStatisticsResponseDtoConverter.convert(musicianEventListResponse.getMusicianEventAssistStatistics()))
                .musiciansAssistInformation(this.musicianAssistInformationResponseToMusicianAssistInformationResponseResponseDtoConverter.convert(musicianEventListResponse.getMusicianAssitsInformation()))
                .events(this.eventResponseListToEventResponseDtoListConverter.convert(musicianEventListResponse.getEvents()))
                .build();
    }

}
