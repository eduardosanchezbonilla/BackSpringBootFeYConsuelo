package com.feyconsuelo.infrastructure.converter.musicianrehearsal;

import com.feyconsuelo.domain.model.musicianevent.MusicianEventRequest;
import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalEntity;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalPK;
import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianEventRequestToMusicianRehearsalEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public MusicianRehearsalEntity convert(final MusicianEventRequest musicianEventRequest) {
        return MusicianRehearsalEntity.builder()
                .id(
                        MusicianRehearsalPK.builder()
                                .musicianId(musicianEventRequest.getMusicianId())
                                .rehearsalId(musicianEventRequest.getEventId())
                                .build()
                )
                .musician(
                        MusicianEntity.builder()
                                .id(musicianEventRequest.getMusicianId())
                                .build()
                )
                .rehearsal(
                        RehearsalEntity.builder()
                                .id(musicianEventRequest.getEventId())
                                .build()
                )
                .updateUserMR(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public MusicianRehearsalEntity deleteEntity(final MusicianRehearsalEntity musicianRehearsalEntity) {
        musicianRehearsalEntity.setDeleteDateMR(LocalDateTime.now());
        musicianRehearsalEntity.setUpdateUserMR(this.tokenInfoExtractorService.getUsername());
        return musicianRehearsalEntity;
    }
}
