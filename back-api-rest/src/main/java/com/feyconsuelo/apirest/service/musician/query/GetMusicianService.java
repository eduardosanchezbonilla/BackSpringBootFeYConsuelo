package com.feyconsuelo.apirest.service.musician.query;

import com.feyconsuelo.apirest.converter.musician.MusicianResponseListToMusicianResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.musician.MusicianResponseToMusicianResponseDtoConverter;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.usecase.musician.GetAllMusicians;
import com.feyconsuelo.domain.usecase.musician.GetMusician;
import com.feyconsuelo.openapi.model.MusicianResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetMusicianService {

    private final GetAllMusicians getAllMusicians;

    private final GetMusician getMusician;

    private final MusicianResponseToMusicianResponseDtoConverter musicianResponseToMusicianResponseDtoConverter;

    private final MusicianResponseListToMusicianResponseDtoListConverter musicianResponseListToMusicianResponseDtoListConverter;

    public ResponseEntity<List<MusicianResponseDto>> getAllMusicians() {
        final List<MusicianResponse> musicianRequestList = this.getAllMusicians.execute();
        if (CollectionUtils.isEmpty(musicianRequestList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.musicianResponseListToMusicianResponseDtoListConverter.convert(musicianRequestList));
    }

    public ResponseEntity<MusicianResponseDto> getMusician(final Long musicianId) {
        final Optional<MusicianResponseDto> musicianResponseDto = this.getMusician.execute(musicianId).map(this.musicianResponseToMusicianResponseDtoConverter::convert);
        return musicianResponseDto.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
