package com.feyconsuelo.infrastructure.converter.rehearsal;

import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventRequestToRehearsalEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public RehearsalEntity convert(final EventRequest eventRequest) {
        return RehearsalEntity.builder()
                .date(eventRequest.getDate())
                .startTime(eventRequest.getStartTime())
                .endTime(eventRequest.getEndTime())
                .description(eventRequest.getDescription())
                .voiceIdList(eventRequest.getVoiceIdList())
                .location(eventRequest.getLocation())
                .municipality(eventRequest.getMunicipality())
                .province(eventRequest.getProvince())
                .modifiedUser(this.tokenInfoExtractorService.getUsername())
                .duration(eventRequest.getDuration())
                .build();
    }

    public RehearsalEntity updateEntity(final RehearsalEntity rehearsalEntity,
                                        final EventRequest eventRequest) {
        rehearsalEntity.setDate(eventRequest.getDate());
        rehearsalEntity.setStartTime(eventRequest.getStartTime());
        rehearsalEntity.setEndTime(eventRequest.getEndTime());
        rehearsalEntity.setDescription(eventRequest.getDescription());
        rehearsalEntity.setVoiceIdList(eventRequest.getVoiceIdList());
        rehearsalEntity.setLocation(eventRequest.getLocation());
        rehearsalEntity.setMunicipality(eventRequest.getMunicipality());
        rehearsalEntity.setProvince(eventRequest.getProvince());
        rehearsalEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());
        rehearsalEntity.setDuration(eventRequest.getDuration());

        return rehearsalEntity;
    }

    public RehearsalEntity deleteEntity(final RehearsalEntity rehearsalEntity) {
        rehearsalEntity.setDeleteDate(LocalDateTime.now());
        rehearsalEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return rehearsalEntity;
    }
}
