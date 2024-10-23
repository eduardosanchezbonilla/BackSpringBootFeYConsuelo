package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.voice.VoiceService;
import com.feyconsuelo.domain.model.musician.MusicianGroupByVoiceRequest;
import com.feyconsuelo.domain.model.musician.MusicianGroupByVoiceResponse;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.domain.usecase.musician.GetMusiciansGroupByVoice;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetMusiciansGroupByVoiceImpl implements GetMusiciansGroupByVoice {

    private final VoiceService voiceService;
    private final MusicianService musicianService;

    private Boolean filterMusician(final MusicianResponse musician, final MusicianGroupByVoiceRequest musicianGroupByVoiceRequest) {
        if (StringUtils.isEmpty(musicianGroupByVoiceRequest.getName())) {
            return Boolean.TRUE;
        } else {
            return (musician.getName() + " " + musician.getSurname()).toUpperCase().contains(musicianGroupByVoiceRequest.getName().toUpperCase());
        }
    }

    private List<MusicianResponse> filterMusicians(final List<MusicianResponse> musicians,
                                                   final MusicianGroupByVoiceRequest musicianGroupByVoiceRequest) {

        return Boolean.TRUE.equals(CollectionUtils.isEmpty(musicians)) ?
                musicians :
                musicians.stream()
                        .filter(musician -> this.filterMusician(musician, musicianGroupByVoiceRequest))
                        .toList();
    }

    @Override
    public List<MusicianGroupByVoiceResponse> execute(final MusicianGroupByVoiceRequest musicianGroupByVoiceRequest) {

        // obtenemos todas las voces
        final List<VoiceResponse> voices = this.voiceService.getAll();

        // obtenemos todos los musicos
        final List<MusicianResponse> musicians = this.musicianService.getAll();

        final List<MusicianResponse> filterMusicians = this.filterMusicians(musicians, musicianGroupByVoiceRequest);

        // recorremos todas las voces y en cada una de ellos metemos los musicos que coincidan en voz
        return voices.stream()
                .map(
                        voice -> MusicianGroupByVoiceResponse.builder()
                                .voice(voice)
                                .musicians(
                                        filterMusicians.stream()
                                                .filter(musician -> musician.getVoice().getId().equals(voice.getId()))
                                                .toList()
                                )
                                .build()
                )
                //.filter(musicianGroupByVoiceResponse -> Boolean.FALSE.equals(musicianGroupByVoiceResponse.getMusicians().isEmpty()))
                .toList();
    }
}
