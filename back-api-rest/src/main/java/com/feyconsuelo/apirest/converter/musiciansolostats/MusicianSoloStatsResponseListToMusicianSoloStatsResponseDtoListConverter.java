package com.feyconsuelo.apirest.converter.musiciansolostats;

import com.feyconsuelo.application.usecase.utils.NumberService;
import com.feyconsuelo.domain.model.musiciansolostats.MusicianSoloStatsResponse;
import com.feyconsuelo.openapi.model.MusicianSolosStatsResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianSoloStatsResponseListToMusicianSoloStatsResponseDtoListConverter {

    private final NumberService numberService;

    public List<MusicianSolosStatsResponseDto> convert(final List<MusicianSoloStatsResponse> musicianSoloStatsResponseList) {
        if (CollectionUtils.isEmpty(musicianSoloStatsResponseList)) {
            return List.of();
        }

        final Integer sumTotalSolos = musicianSoloStatsResponseList.stream()
                .map(MusicianSoloStatsResponse::getTotalSolos)
                .reduce(0, Integer::sum);
        final Integer sumTotalMainSolos = musicianSoloStatsResponseList.stream()
                .map(MusicianSoloStatsResponse::getTotalMainSolos)
                .reduce(0, Integer::sum);
        final Integer sumTotalSecondarySolos = musicianSoloStatsResponseList.stream()
                .map(MusicianSoloStatsResponse::getTotalSecondarySolos)
                .reduce(0, Integer::sum);

        return musicianSoloStatsResponseList.stream()
                .map(musicianSoloStatsResponse ->
                        MusicianSolosStatsResponseDto.builder()
                                .id(musicianSoloStatsResponse.getId())
                                .name(musicianSoloStatsResponse.getName())
                                .surname(musicianSoloStatsResponse.getSurname())
                                .totalSolos(musicianSoloStatsResponse.getTotalSolos())
                                .percentageSolos(sumTotalSolos == 0 ? 0 : this.numberService.round(((double) musicianSoloStatsResponse.getTotalSolos() / sumTotalSolos) * 100))
                                .totalMainSolos(musicianSoloStatsResponse.getTotalMainSolos())
                                .percentageMainSolos(sumTotalMainSolos == 0 ? 0 : this.numberService.round(((double) musicianSoloStatsResponse.getTotalMainSolos() / sumTotalMainSolos) * 100))
                                .totalSecondarySolos(musicianSoloStatsResponse.getTotalSecondarySolos())
                                .percentageSecondarySolos(sumTotalSecondarySolos == 0 ? 0 : this.numberService.round(((double) musicianSoloStatsResponse.getTotalSecondarySolos() / sumTotalSecondarySolos) * 100))
                                .build()
                )
                .sorted(Comparator.comparing(MusicianSolosStatsResponseDto::getTotalSolos, Comparator.reverseOrder())
                        .thenComparing(MusicianSolosStatsResponseDto::getTotalMainSolos, Comparator.reverseOrder()))
                .toList();
    }

}
