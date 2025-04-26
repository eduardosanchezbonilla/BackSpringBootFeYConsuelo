package com.feyconsuelo.application.usecase.rehearsal;

import com.feyconsuelo.application.service.musicianrehearsal.MusicianRehearsalService;
import com.feyconsuelo.application.service.rehearsal.RehearsalService;
import com.feyconsuelo.domain.model.event.EventResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetAllRehearsalImpl {

    private final RehearsalService rehearsalService;

    private final MusicianRehearsalService musicianRehearsalService;

    public List<EventResponse> execute(final LocalDate startDate, final LocalDate endDate, final Optional<Long> musicianId) {
        if (musicianId.isEmpty()) {
            return this.rehearsalService.getAll(startDate, endDate);
        } else {
            return this.musicianRehearsalService.getAllMusicianRehearsal(
                    musicianId.get(),
                    startDate,
                    endDate
            );
        }
    }
}
