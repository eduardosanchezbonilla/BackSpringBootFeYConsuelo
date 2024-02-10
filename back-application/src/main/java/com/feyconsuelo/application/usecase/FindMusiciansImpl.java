package com.feyconsuelo.application.usecase;

import com.feyconsuelo.application.repository.MusicianRepository;
import com.feyconsuelo.domain.entity.musician.FindMusiciansRequest;
import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.domain.usecase.FindMusicians;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class FindMusiciansImpl implements FindMusicians {

    private final MusicianRepository musicianRepository;

    @Override
    public List<Musician> execute(final FindMusiciansRequest request) {
        return this.musicianRepository.find(request);
    }
}
