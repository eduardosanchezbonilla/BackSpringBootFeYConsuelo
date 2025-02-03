package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.apirest.converter.voice.VoiceResponseToVoiceResponseDtoConverter;
import com.feyconsuelo.domain.model.event.EventMusicianAssistanceResponse;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.openapi.model.EventMusicianAssistanceResponseDto;
import com.feyconsuelo.openapi.model.MusicianGroupByVoiceResponseDto;
import com.feyconsuelo.openapi.model.MusicianResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventMusicianAssistanceResponseToEventMusicianAssistanceResponseDtoConverter {

    private final EventResponseToEventResponseDtoConverter eventResponseToEventResponseDtoConverter;
    private final VoiceResponseToVoiceResponseDtoConverter voiceResponseToVoiceResponseDtoConverter;

    private List<MusicianGroupByVoiceResponseDto> getMusiciansGroupByVoice(final EventMusicianAssistanceResponse eventMusicianAssistanceResponse) {
        if (CollectionUtils.isEmpty(eventMusicianAssistanceResponse.getMusicians())) {
            return List.of();
        } else {
            final List<MusicianResponseDto> musicians = eventMusicianAssistanceResponse.getMusicians().stream()
                    .filter(musicianResponse ->
                            musicianResponse.getVoice() != null &&
                                    eventMusicianAssistanceResponse.getEvent().getVoiceList().stream()
                                            .map(VoiceResponse::getId)
                                            .toList()
                                            .contains(musicianResponse.getVoice().getId())
                    )
                    .map(
                            musicianResponse -> MusicianResponseDto.builder()
                                    .id(musicianResponse.getId())
                                    .dni(musicianResponse.getDni())
                                    .name(musicianResponse.getName())
                                    .surname(musicianResponse.getSurname())
                                    .email(musicianResponse.getEmail())
                                    .voice(this.voiceResponseToVoiceResponseDtoConverter.convert(musicianResponse.getVoice()))
                                    .assistLastRehearsal(musicianResponse.getAssistLastRehearsal())
                                    .assistBus(musicianResponse.getAssistBus())
                                    .image(musicianResponse.getImage())
                                    .formationPositionX(musicianResponse.getFormationPositionX())
                                    .formationPositionY(musicianResponse.getFormationPositionY())
                                    .build()
                    )
                    .toList();

            // ahora debemos agrupar por voz y crear el array de MusicianGroupByVoiceResponseDto
            return musicians.stream()
                    .collect(Collectors.groupingBy(MusicianResponseDto::getVoice))
                    .entrySet().stream()
                    .map(entry -> MusicianGroupByVoiceResponseDto.builder()
                            .voice(entry.getKey())
                            .musicians(entry.getValue())
                            .build())
                    .sorted(Comparator.comparing(musicianGroupByVoice -> musicianGroupByVoice.getVoice().getOrder()))
                    .toList();
        }

    }

    public EventMusicianAssistanceResponseDto convert(final EventMusicianAssistanceResponse eventMusicianAssistanceResponse) {
        return EventMusicianAssistanceResponseDto.builder()
                .event(this.eventResponseToEventResponseDtoConverter.convert(eventMusicianAssistanceResponse.getEvent()))
                .musiciansGroupByVoice(this.getMusiciansGroupByVoice(eventMusicianAssistanceResponse))
                .build();
    }

}
