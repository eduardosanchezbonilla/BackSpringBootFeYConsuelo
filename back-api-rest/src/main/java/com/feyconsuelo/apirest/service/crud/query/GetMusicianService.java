package com.feyconsuelo.apirest.service.crud.query;

import com.feyconsuelo.apirest.mapper.MusicianMapper;
import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.domain.usecase.GetAllMusicians;
import com.feyconsuelo.domain.usecase.GetMusician;
import com.feyconsuelo.openapi.model.MusicianResponseDTO;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetMusicianService {

    private final GetAllMusicians getAllMusicians;

    private final GetMusician getMusician;

    //private final FindMusicians findMusicians;

    private final MusicianMapper musicianMapper;

    public ResponseEntity<List<MusicianResponseDTO>> getAllMusicians() {
        final List<Musician> musicianList = this.getAllMusicians.execute();

        if (CollectionUtils.isEmpty(musicianList)) {
            return ResponseEntity.noContent().build();
        }

        return ResponseEntity.ok(this.musicianMapper.map(musicianList));
    }
    
    public ResponseEntity<MusicianResponseDTO> getMusician(final String musicianId) {
        final Optional<MusicianResponseDTO> musicianResponseDTO = this.getMusician.execute(musicianId).map(this.musicianMapper::map);
        return musicianResponseDTO.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

    /*@Override
    public ResponseEntity<List<MusicianResponseDTO>> findMusicianRegistries(final String domain, final String subdomain, final String providerInput, final String providerOutput, final String valueInput, final String valueOutput, final String description) {
        final List<Musician> musicianList = this.findMusicians.execute(FindMusiciansRequest
                .builder()
                .domain(domain)
                .subdomain(subdomain)
                .providerInput(providerInput)
                .providerOutput(providerOutput)
                .valueInput(valueInput)
                .valueOutput(valueOutput)
                .description(description)
                .build());

        if (CollectionUtils.isEmpty(musicianList)) {
            return ResponseEntity.noContent().build();
        }

        return ResponseEntity.ok(this.musicianMapper.map(musicianList));
    }*/

}
