package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.usecase.musician.GetAllMusicians;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetAllMusiciansImpl implements GetAllMusicians {

    private final MusicianService musicianService;

    @Override
    public List<MusicianResponse> execute(final Boolean unregistred) {
        return this.musicianService.getAll(unregistred);
    }
}
