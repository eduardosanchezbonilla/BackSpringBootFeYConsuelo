package com.feyconsuelo.apirest.converter.musicianevent;

import com.feyconsuelo.apirest.converter.event.EventResponseListToEventResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.statistics.EventAssistStatisticsResponseToEventAssistStatisticsResponseDtoConverter;
import com.feyconsuelo.apirest.converter.statistics.MusicianAssistInformationResponseToMusicianAssistInformationResponseResponseDtoConverter;
import com.feyconsuelo.apirest.converter.statistics.MusicianEventAssistStatisticsResponseToMusicianEventAssistStatisticsResponseDtoConverter;
import com.feyconsuelo.apirest.converter.statistics.RepertoireMarchEventStatisticResponseToRepertoireMarchEventStatisticResponseDtoConverter;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventListResponse;
import com.feyconsuelo.openapi.model.EventResponseDto;
import com.feyconsuelo.openapi.model.MusicianEventListResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianEventListResponseToMusicianEventListResponseDtoConverter {

    private final EventResponseListToEventResponseDtoListConverter eventResponseListToEventResponseDtoListConverter;
    private final MusicianEventAssistStatisticsResponseToMusicianEventAssistStatisticsResponseDtoConverter musicianEventAssistStatisticsResponseToMusicianEventAssistStatisticsResponseDtoConverter;
    private final EventAssistStatisticsResponseToEventAssistStatisticsResponseDtoConverter eventAssistStatisticsResponseToEventAssistStatisticsResponseDtoConverter;
    private final MusicianAssistInformationResponseToMusicianAssistInformationResponseResponseDtoConverter musicianAssistInformationResponseToMusicianAssistInformationResponseResponseDtoConverter;
    private final RepertoireMarchEventStatisticResponseToRepertoireMarchEventStatisticResponseDtoConverter repertoireMarchEventStatisticResponseToRepertoireMarchEventStatisticResponseDtoConverter;

    public MusicianEventListResponseDto convert(final MusicianEventListResponse musicianEventListResponse,
                                                final Boolean allEvents) {

        final List<EventResponseDto> eventResponseDtoList = this.eventResponseListToEventResponseDtoListConverter.convert(musicianEventListResponse.getEvents());

        return MusicianEventListResponseDto.builder()
                .eventAssistStatistic(this.eventAssistStatisticsResponseToEventAssistStatisticsResponseDtoConverter.convert(musicianEventListResponse.getEventAssistStatisticsResponse()))
                .musicianEventAssistStatistic(this.musicianEventAssistStatisticsResponseToMusicianEventAssistStatisticsResponseDtoConverter.convert(musicianEventListResponse.getMusicianEventAssistStatistics()))
                .musiciansAssistInformation(this.musicianAssistInformationResponseToMusicianAssistInformationResponseResponseDtoConverter.convert(musicianEventListResponse.getMusicianAssitsInformation()))
                .repertoireMarchEventStatistic(this.repertoireMarchEventStatisticResponseToRepertoireMarchEventStatisticResponseDtoConverter.convert(musicianEventListResponse.getRepertoireMarchEventStatistic()))
                .events(
                        Boolean.FALSE.equals(CollectionUtils.isEmpty(eventResponseDtoList)) && Boolean.FALSE.equals(allEvents)
                                ? List.of(eventResponseDtoList.stream().sorted(Comparator.comparing(EventResponseDto::getDate).reversed()).toList().get(0))
                                : eventResponseDtoList
                )
                .build();
    }

}
