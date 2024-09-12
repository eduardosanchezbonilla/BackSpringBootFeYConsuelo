package com.feyconsuelo.apirest.converter.musician;

import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.openapi.model.MusicianResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianResponseListToMusicianResponseDtoListConverter {

    private final MusicianResponseToMusicianResponseDtoConverter musicianResponseToMusicianResponseDtoConverter;

    public List<MusicianResponseDto> convert(final List<MusicianResponse> musicianResponseList) {
        if (CollectionUtils.isEmpty(musicianResponseList)) {
            return List.of();
        }
        return musicianResponseList.stream()
                .map(this.musicianResponseToMusicianResponseDtoConverter::convert)
                .toList();
    }

}
