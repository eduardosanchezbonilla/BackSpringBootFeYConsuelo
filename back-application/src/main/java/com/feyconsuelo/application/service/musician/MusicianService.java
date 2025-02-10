package com.feyconsuelo.application.service.musician;

import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.domain.model.musician.MusicianResponse;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

public interface MusicianService {

    List<MusicianResponse> getAll(final Boolean unregistred);

    Optional<MusicianResponse> get(Long musicianId, final boolean isThumbnail);

    Optional<MusicianResponse> getByDni(String dni, final boolean isThumbnail);

    List<MusicianResponse> getByBirthdayDate(LocalDate birthdayDate);

    List<MusicianResponse> getByVoice(Long voiceId);

    MusicianResponse insert(MusicianRequest musicianRequest);

    MusicianResponse update(Long musicianId, MusicianRequest musicianRequest);

    void delete(Long musicianId);

    void logicalDelete(Long musicianId);

    void updateLastNotificationNonAssistsStreakRehearsals(Long musicianId, LocalDateTime date);

}
