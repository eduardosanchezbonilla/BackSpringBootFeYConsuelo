package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.musicianrehearsal.MusicianRehearsalService;
import com.feyconsuelo.application.service.rehearsal.RehearsalService;
import com.feyconsuelo.application.service.voice.VoiceService;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.musician.MusicianGroupByVoiceRequest;
import com.feyconsuelo.domain.model.musician.MusicianGroupByVoiceResponse;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventResponse;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.domain.usecase.musician.GetMusiciansGroupByVoice;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetMusiciansGroupByVoiceImpl implements GetMusiciansGroupByVoice {

    private final VoiceService voiceService;
    private final MusicianService musicianService;
    private final MusicianRehearsalService musicianRehearsalService;
    private final RehearsalService rehearsalService;

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
        final List<MusicianResponse> musicians = this.musicianService.getAll(musicianGroupByVoiceRequest.getUnregistred());

        // obtengo el ultimo ensayo realizado hasta este momento
        final Optional<EventResponse> eventResponse = this.rehearsalService.findLastRehearsalUntilDateTime(LocalDateTime.now().plusHours(2));

        // obtenemos la asistencia de los musicos al ultimo ensayo que se haya realizado hasta este momento
        final List<MusicianEventResponse> musicianEventResponseList;

        if (eventResponse.isPresent()) {
            musicianEventResponseList = this.musicianRehearsalService.findAllActivesMusiciansByRehearsalId(eventResponse.get().getId(), false);
        } else {
            musicianEventResponseList = List.of();
        }

        // asigno a cada musico su asistencia al ultimo ensayo
        musicians.forEach(
                musician -> {
                    // asigno ensayo solo si la fecha de incorporacion del musico es menor o igual a la fecha del ensayo, y ademas, la voz del musico enta entre las voces del ensayo
                    if (musician.getRegistrationDate() != null &&
                            eventResponse.isPresent() &&
                            !musician.getRegistrationDate().toLocalDate().isAfter(eventResponse.get().getDate()) &&
                            eventResponse.get().getVoiceIdList().contains(musician.getVoice().getId().intValue())
                    ) {
                        musician.setAssistLastRehearsal(
                                musicianEventResponseList.stream()
                                        .anyMatch(musicianEventResponse -> musicianEventResponse.getMusicianResponse().getId().equals(musician.getId()))
                        );
                        musician.setIdLastRehearsal(eventResponse.map(EventResponse::getId).orElse(null));
                        musician.setDateLastRehearsal(eventResponse.map(EventResponse::getDate).orElse(null));
                    }
                }
        );

        // filtramos los musicos por voz
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
