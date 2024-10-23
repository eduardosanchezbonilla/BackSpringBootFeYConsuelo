package com.feyconsuelo.apirest.converter.musician;

import com.feyconsuelo.domain.model.musician.MusicianGroupByVoiceResponse;
import com.feyconsuelo.openapi.model.MusicianGroupByVoiceResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianGroupByVoiceListResponseToMusicianGroupByVoiceListResponseDtoConverter {

    private final MusicianGroupByVoiceResponseToMusicianGroupByVoiceResponseDtoConverter musicianGroupByVoiceResponseToMusicianGroupByVoiceResponseDtoConverter;

    public List<MusicianGroupByVoiceResponseDto> convert(final List<MusicianGroupByVoiceResponse> musicianGroupByVoiceResponseList) {
        if (CollectionUtils.isEmpty(musicianGroupByVoiceResponseList)) {
            return List.of();
        }
        return musicianGroupByVoiceResponseList.stream()
                .map(this.musicianGroupByVoiceResponseToMusicianGroupByVoiceResponseDtoConverter::convert)
                .sorted(Comparator.comparing(group -> group.getVoice().getOrder()))
                .toList();
    }

}
