package com.feyconsuelo.infrastructure.service.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.infrastructure.converter.musician.MusicianEntityListToMusicianResponseListConverter;
import com.feyconsuelo.infrastructure.converter.musician.MusicianEntityToMusicianResponseConverter;
import com.feyconsuelo.infrastructure.converter.musician.MusicianRequestToMusicianEntityConverter;
import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import com.feyconsuelo.infrastructure.repository.MusicianRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class MusicianServiceImpl implements MusicianService {

    private final MusicianRepository musicianRepository;
    private final MusicianRequestToMusicianEntityConverter musicianToMusicianEntityConverter;
    private final MusicianEntityListToMusicianResponseListConverter musicianEntityListToMusicianResponseListConverter;
    private final MusicianEntityToMusicianResponseConverter musicianEntityToMusicianResponseConverter;

    @Override
    public void delete(final Long musicianId) {
        this.musicianRepository.deleteById(musicianId);
    }

    @Override
    public void logicalDelete(final Long musicianId) {

        final var musician = this.musicianRepository.findMusicianActiveById(musicianId);

        if (musician.isEmpty()) {
            throw new NotFoundException("Musician to delete not found");
        }

        this.musicianRepository.save(this.musicianToMusicianEntityConverter.deleteEntity(musician.get()));
    }

    @Override
    public List<MusicianResponse> getAll() {
        final List<MusicianEntity> musicians = this.musicianRepository.findAllActives();
        return this.musicianEntityListToMusicianResponseListConverter.convert(musicians);
    }

    @Override
    public Optional<MusicianResponse> get(final Long musicianId) {
        final var musician = this.musicianRepository.findMusicianActiveById(musicianId);
        return musician.map(this.musicianEntityToMusicianResponseConverter::convert);
    }

    @Override
    public Optional<MusicianResponse> getByDni(final String dni) {
        final var musician = this.musicianRepository.findMusicianActiveByDni(dni);
        return musician.map(this.musicianEntityToMusicianResponseConverter::convert);
    }

    @Override
    public MusicianResponse insert(final MusicianRequest musicianRequest) {
        return this.musicianEntityToMusicianResponseConverter.convert(
                this.musicianRepository.save(
                        this.musicianToMusicianEntityConverter.convert(musicianRequest)
                )
        );
    }

    @Override
    public MusicianResponse update(final Long musicianId,
                                   final MusicianRequest musicianRequest
    ) {

        final var musician = this.musicianRepository.findMusicianActiveById(musicianId);

        if (musician.isEmpty()) {
            throw new NotFoundException("Musician register to update not found");
        }

        return this.musicianEntityToMusicianResponseConverter.convert(
                this.musicianRepository.save(
                        this.musicianToMusicianEntityConverter.updateEntity(musician.get(), musicianRequest)
                )
        );
    }

}
