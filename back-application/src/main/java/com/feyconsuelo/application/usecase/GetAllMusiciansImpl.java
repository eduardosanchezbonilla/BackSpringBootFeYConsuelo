package com.feyconsuelo.application.usecase;

import com.feyconsuelo.application.repository.MusicianRepository;
import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.domain.usecase.GetAllMusicians;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetAllMusiciansImpl implements GetAllMusicians {

    private final MusicianRepository musicianRepository;

    @Override
    public List<Musician> execute() {
        return this.musicianRepository.getAll();
    }
}
