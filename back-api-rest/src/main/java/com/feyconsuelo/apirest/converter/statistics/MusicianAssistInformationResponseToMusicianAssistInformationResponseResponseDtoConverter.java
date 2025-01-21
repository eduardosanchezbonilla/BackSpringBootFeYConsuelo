package com.feyconsuelo.apirest.converter.statistics;

import com.feyconsuelo.domain.model.statistics.AllMusicianEventAssistStatisticsResponse;
import com.feyconsuelo.openapi.model.MusicianAssistInformationResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianAssistInformationResponseToMusicianAssistInformationResponseResponseDtoConverter {

    public List<MusicianAssistInformationResponseDto> convert(final List<AllMusicianEventAssistStatisticsResponse> allMusicianEventAssistStatisticsResponseList) {
        if (CollectionUtils.isEmpty(allMusicianEventAssistStatisticsResponseList)) {
            return List.of();
        }
        return allMusicianEventAssistStatisticsResponseList.stream()
                .map(musicianInfo -> MusicianAssistInformationResponseDto.builder()
                        .id(musicianInfo.getMusicianId())
                        .name(musicianInfo.getMusicianName())
                        .surname(musicianInfo.getMusicianSurname())
                        .totalRehearsal(musicianInfo.getTotalRehearsal())
                        .musicianAssistsRehearsal(musicianInfo.getMusicianAssistsRehearsal())
                        .musicianPercentageAssistsRehearsal(musicianInfo.getMusicianPercentageAssistsRehearsal())
                        .build()
                )
                .sorted(Comparator.comparing(MusicianAssistInformationResponseDto::getMusicianPercentageAssistsRehearsal))
                .toList();

    }

}
