package com.feyconsuelo.application.usecase.rehearsal;

import com.feyconsuelo.application.service.musicianrehearsal.MusicianRehearsalService;
import com.feyconsuelo.application.service.rehearsal.RehearsalService;
import com.feyconsuelo.domain.model.event.EventResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetAllRehearsalImpl {

    private final RehearsalService rehearsalService;

    private final MusicianRehearsalService musicianRehearsalService;

    private List<EventResponse> getMusicianRehearsal(final LocalDate startDate, final LocalDate endDate, final Optional<Long> musicianId) {
        List<EventResponse> musicianRehearsal = new ArrayList<>();
        if (musicianId.isPresent()) {
            musicianRehearsal = this.musicianRehearsalService.getAll(musicianId.get(), startDate, endDate);
        }
        return musicianRehearsal;
    }

    public List<EventResponse> execute(final LocalDate startDate, final LocalDate endDate, final Optional<Long> musicianId) {
        final List<EventResponse> musicianRehearsal = this.getMusicianRehearsal(startDate, endDate, musicianId);
        final List<EventResponse> rehearsal = this.rehearsalService.getAll(startDate, endDate);

        if (CollectionUtils.isEmpty(musicianRehearsal)) {
            return rehearsal;
        }

        // para cada rehearsal, miro si existe en musicianRehearsal. Si existe, le cambio del clsClass
        for (final EventResponse eventResponse : rehearsal) {
            for (final EventResponse musicianEventResponse : musicianRehearsal) {
                if (eventResponse.getId().equals(musicianEventResponse.getId())) {
                    eventResponse.setClsClass(musicianEventResponse.getClsClass());
                    eventResponse.setAssist(Boolean.TRUE);
                }
            }
        }

        return rehearsal;
    }
}
