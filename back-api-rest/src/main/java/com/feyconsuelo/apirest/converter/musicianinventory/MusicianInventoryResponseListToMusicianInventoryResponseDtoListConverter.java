package com.feyconsuelo.apirest.converter.musicianinventory;

import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryResponse;
import com.feyconsuelo.openapi.model.MusicianInventoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianInventoryResponseListToMusicianInventoryResponseDtoListConverter {

    private final MusicianInventoryResponseToMusicianInventoryResponseDtoConverter musicianInventoryResponseToMusicianInventoryResponseDtoConverter;

    public List<MusicianInventoryResponseDto> convert(final List<MusicianInventoryResponse> musicianInventoryResponseList) {
        if (CollectionUtils.isEmpty(musicianInventoryResponseList)) {
            return List.of();
        }
        return musicianInventoryResponseList.stream()
                .map(this.musicianInventoryResponseToMusicianInventoryResponseDtoConverter::convert)
                .toList();
    }

}
