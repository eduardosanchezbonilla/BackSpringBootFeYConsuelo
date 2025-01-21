package com.feyconsuelo.infrastructure.converter.statistics;

import com.feyconsuelo.domain.model.statistics.AllMusicianEventAssistStatisticsResponse;
import com.feyconsuelo.infrastructure.entities.statistics.AllMusicianEventAssistStatistics;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class AllMusicianEventAssistStatisticsToAllMusicianEventAssistStatisticsResponseConverter {

    public List<AllMusicianEventAssistStatisticsResponse> convert(final List<AllMusicianEventAssistStatistics> entityList) {
        if (CollectionUtils.isEmpty(entityList)) {
            return List.of();
        } else {
            return entityList.stream()
                    .map(entity ->
                            AllMusicianEventAssistStatisticsResponse.builder()
                                    .musicianId(entity.getMusicianId())
                                    .musicianName(entity.getMusicianName())
                                    .musicianSurname(entity.getMusicianSurname())
                                    .totalRehearsal(entity.getTotalRehearsal())
                                    .musicianAssistsRehearsal(entity.getMusicianAssistsRehearsal())
                                    .musicianPercentageAssistsRehearsal(entity.getMusicianPercentageAssistsRehearsal())
                                    .build()
                    )
                    .toList();
        }
    }

}
