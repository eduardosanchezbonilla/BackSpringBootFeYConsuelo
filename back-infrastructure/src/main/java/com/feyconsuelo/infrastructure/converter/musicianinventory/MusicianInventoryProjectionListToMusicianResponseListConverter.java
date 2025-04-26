package com.feyconsuelo.infrastructure.converter.musicianinventory;

import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.infrastructure.converter.musician.MusicianStringToMusicianResponseConverter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianInventoryProjectionListToMusicianResponseListConverter {

    private final MusicianStringToMusicianResponseConverter musicianStringToMusicianResponseConverter;

    public List<MusicianResponse> convert(final List<String> musicianInventoryEntityList) {
        if (CollectionUtils.isEmpty(musicianInventoryEntityList)) {
            return List.of();
        }
        return musicianInventoryEntityList.stream()
                .map(this.musicianStringToMusicianResponseConverter::convert)
                .filter(Objects::nonNull)
                .distinct()
                .toList();
    }
}
