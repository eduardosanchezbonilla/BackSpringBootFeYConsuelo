package com.feyconsuelo.infrastructure.jpa.repository;

import com.feyconsuelo.application.repository.MusicianRepository;
import com.feyconsuelo.domain.entity.musician.FindMusiciansRequest;
import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.infrastructure.jpa.entities.MusicianEntity;
import com.feyconsuelo.infrastructure.jpa.mapper.MusicianEntityMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Slf4j
@Repository
@RequiredArgsConstructor
public class MusicianRepositoryImpl implements MusicianRepository {

    private final MusicianJpaRepository musicianJpaRepository;

    private final MusicianEntityMapper musicianEntityMapper;

    @Override
    public void delete(final String musicianId) {
        this.musicianJpaRepository.deleteById(musicianId);
    }

    @Override
    public List<Musician> find(final FindMusiciansRequest request) {
        return this.musicianEntityMapper.map(this.musicianJpaRepository.findMusicianRegistries(request));
    }

    @Override
    public List<Musician> getAll() {
        final List<MusicianEntity> musicians = this.musicianJpaRepository.findAll();
        final List<Musician> musiciansList = this.musicianEntityMapper.map(musicians);
        return musiciansList;
    }

    @Override
    public Optional<Musician> get(final String musicianId) {
        return this.musicianJpaRepository.findById(musicianId)
                .map(this.musicianEntityMapper::map);
    }

    @Override
    public Musician save(final Musician musician) {
        return this.musicianEntityMapper.map(
                this.musicianJpaRepository.save(
                        this.musicianEntityMapper.map(musician)
                )
        );
    }
}
