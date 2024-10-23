package com.feyconsuelo.apirest.converter.musicianinventory;

import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryResponse;
import com.feyconsuelo.openapi.model.MusicianInventoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianInventoryResponseToMusicianInventoryResponseDtoConverter {

    public MusicianInventoryResponseDto convert(final MusicianInventoryResponse musicianInventoryResponse) {
        return MusicianInventoryResponseDto.builder()
                .id(musicianInventoryResponse.getInventoryId())
                .name(musicianInventoryResponse.getInventoryName())
                .image(musicianInventoryResponse.getInventoryImage())
                .assigned(musicianInventoryResponse.getAssigned())
                .build();
    }

}
