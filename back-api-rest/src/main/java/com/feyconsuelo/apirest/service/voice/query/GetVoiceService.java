package com.feyconsuelo.apirest.service.voice.query;

import com.feyconsuelo.apirest.converter.voice.VoiceResponseListToVoiceResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.voice.VoiceResponseToVoiceResponseDtoConverter;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.domain.usecase.voice.GetAllVoices;
import com.feyconsuelo.domain.usecase.voice.GetVoice;
import com.feyconsuelo.openapi.model.VoiceResponseDto;
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
public class GetVoiceService {

    private final GetAllVoices getAllVoices;

    private final GetVoice getVoice;

    private final VoiceResponseToVoiceResponseDtoConverter voiceResponseToVoiceResponseDtoConverter;

    private final VoiceResponseListToVoiceResponseDtoListConverter voiceResponseListToVoiceResponseDtoListConverter;

    public ResponseEntity<List<VoiceResponseDto>> getAllVoices() {
        final List<VoiceResponse> voiceResponseList = this.getAllVoices.execute();
        if (CollectionUtils.isEmpty(voiceResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.voiceResponseListToVoiceResponseDtoListConverter.convert(voiceResponseList));
    }

    public ResponseEntity<VoiceResponseDto> getVoice(final Long voiceId) {
        final Optional<VoiceResponseDto> voiceResponseDto = this.getVoice.execute(voiceId).map(this.voiceResponseToVoiceResponseDtoConverter::convert);
        return voiceResponseDto.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
