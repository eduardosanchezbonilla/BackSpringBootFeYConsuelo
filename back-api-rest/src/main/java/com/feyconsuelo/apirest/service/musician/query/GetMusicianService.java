package com.feyconsuelo.apirest.service.musician.query;

import com.feyconsuelo.apirest.converter.musician.MusicianGroupByVoiceListResponseToMusicianGroupByVoiceListResponseDtoConverter;
import com.feyconsuelo.apirest.converter.musician.MusicianResponseListToMusicianResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.musician.MusicianResponseToMusicianResponseDtoConverter;
import com.feyconsuelo.domain.model.musician.MusicianGroupByVoiceRequest;
import com.feyconsuelo.domain.model.musician.MusicianGroupByVoiceResponse;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.usecase.musician.GetAllMusicians;
import com.feyconsuelo.domain.usecase.musician.GetMusician;
import com.feyconsuelo.domain.usecase.musician.GetMusicianFromDni;
import com.feyconsuelo.domain.usecase.musician.GetMusiciansGroupByVoice;
import com.feyconsuelo.openapi.model.MusicianGroupByVoiceResponseDto;
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

    private final GetMusicianFromDni getMusicianFromDni;

    private final GetMusiciansGroupByVoice getMusiciansGroupByVoice;

    private final MusicianResponseToMusicianResponseDtoConverter musicianResponseToMusicianResponseDtoConverter;

    private final MusicianResponseListToMusicianResponseDtoListConverter musicianResponseListToMusicianResponseDtoListConverter;

    private final MusicianGroupByVoiceListResponseToMusicianGroupByVoiceListResponseDtoConverter musicianGroupByVoiceListResponseToMusicianGroupByVoiceListResponseDtoConverter;

    public ResponseEntity<List<MusicianResponseDto>> getAllMusicians(final Boolean unregistred) {
        final List<MusicianResponse> musicians = this.getAllMusicians.execute(unregistred);
        if (CollectionUtils.isEmpty(musicians)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.musicianResponseListToMusicianResponseDtoListConverter.convert(musicians));
    }

    public ResponseEntity<MusicianResponseDto> getMusician(final Long musicianId) {
        final Optional<MusicianResponseDto> musician = this.getMusician.execute(musicianId).map(this.musicianResponseToMusicianResponseDtoConverter::convert);
        return musician.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

    public ResponseEntity<List<MusicianGroupByVoiceResponseDto>> getMusiciansGroupByVoice(final MusicianGroupByVoiceRequest musicianGroupByVoiceRequest) {
        final List<MusicianGroupByVoiceResponse> musicians = this.getMusiciansGroupByVoice.execute(musicianGroupByVoiceRequest);
        if (CollectionUtils.isEmpty(musicians)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.musicianGroupByVoiceListResponseToMusicianGroupByVoiceListResponseDtoConverter.convert(musicians));
    }

    public ResponseEntity<MusicianResponseDto> getMusicianFromDni(final String musicianDni) {
        final Optional<MusicianResponseDto> musician = this.getMusicianFromDni.execute(musicianDni).map(this.musicianResponseToMusicianResponseDtoConverter::convert);
        return musician.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
