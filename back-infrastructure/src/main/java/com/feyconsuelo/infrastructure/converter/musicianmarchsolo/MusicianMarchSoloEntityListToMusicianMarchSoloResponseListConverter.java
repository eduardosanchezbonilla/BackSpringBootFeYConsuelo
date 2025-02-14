package com.feyconsuelo.infrastructure.converter.musicianmarchsolo;

import com.feyconsuelo.domain.model.musicianmarchsolo.MusicianMarchSoloResponse;
import com.feyconsuelo.domain.model.musicianmarchsolo.MusicianSoloResponse;
import com.feyconsuelo.infrastructure.entities.musicianmarchsolo.MusicianMarchSolo;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.stream.Collectors;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianMarchSoloEntityListToMusicianMarchSoloResponseListConverter {

    public List<MusicianMarchSoloResponse> convert(final List<MusicianMarchSolo> musicianMarchSoloEntityList) {
        if (CollectionUtils.isEmpty(musicianMarchSoloEntityList)) {
            return List.of();
        }

        return musicianMarchSoloEntityList.stream()
                .collect(Collectors.groupingBy(MusicianMarchSolo::getMarchName,
                                // Además, mapeamos cada dto a un objeto Solo
                                Collectors.mapping(dto ->
                                                MusicianSoloResponse.builder()
                                                        .soloName(dto.getSoloName())
                                                        .soloOrder(dto.getSoloOrder())
                                                        .mainSoloistOrder(dto.getMainSoloistOrder())
                                                        .minMainSoloistOrder(dto.getMinMainOrder())
                                                        .maxMainSoloistOrder(dto.getMaxMainOrder())
                                                        .secondarySoloistOrder(dto.getSecondarySoloistOrder())
                                                        .minSecondarySoloistOrder(dto.getMinSecondaryOrder())
                                                        .maxSecondarySoloistOrder(dto.getMaxSecondaryOrder())
                                                        .build()
                                        , Collectors.toList())
                        )
                )
                .entrySet().stream()
                .map(entry -> MusicianMarchSoloResponse.builder()
                        .marchName(entry.getKey())
                        .solos(entry.getValue())
                        .build()
                )
                .toList();
    }
}
