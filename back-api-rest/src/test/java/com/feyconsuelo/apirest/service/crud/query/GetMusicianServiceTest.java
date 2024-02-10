package com.feyconsuelo.apirest.service.crud.query;

import com.feyconsuelo.apirest.mapper.MusicianMapper;
import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.domain.usecase.FindMusicians;
import com.feyconsuelo.domain.usecase.GetAllMusicians;
import com.feyconsuelo.domain.usecase.GetMusician;
import com.feyconsuelo.openapi.model.MusicianResponseDTO;
import io.github.glytching.junit.extension.random.Random;
import io.github.glytching.junit.extension.random.RandomBeansExtension;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, RandomBeansExtension.class})
class GetMusicianServiceTest {

    @InjectMocks
    private GetMusicianService getMusicianService;

    @Mock
    private GetAllMusicians getAllMusicians;

    @Mock
    private GetMusician getMusician;

    @Mock
    private FindMusicians findMusicians;

    @Mock
    private MusicianMapper musicianMapper;

    @Test
    void getAllMusiciansTest(@Random(size = 2, type = MusicianResponseDTO.class) final List<MusicianResponseDTO> musicianResponseDTOList,
                             @Random(size = 2, type = Musician.class) final List<Musician> musicianList) {

        when(this.getAllMusicians.execute()).thenReturn(musicianList);
        when(this.musicianMapper.map(musicianList)).thenReturn(musicianResponseDTOList);

        final ResponseEntity<List<MusicianResponseDTO>> result = this.getMusicianService.getAllMusicians();

        assertThat(result.getBody()).isEqualTo(musicianResponseDTOList);
        assertThat(result.getStatusCode()).isEqualTo(HttpStatus.OK);

    }

    @Test
    void getAllMusiciansNoContentTest() {

        when(this.getAllMusicians.execute()).thenReturn(List.of());

        final ResponseEntity<List<MusicianResponseDTO>> result = this.getMusicianService.getAllMusicians();

        assertThat(result.getStatusCode()).isEqualTo(HttpStatus.NO_CONTENT);

    }

    @Test
    void getMusicianTest(@Random final Musician musician,
                         @Random final MusicianResponseDTO musicianResponseDTO) {

        when(this.getMusician.execute("id")).thenReturn(Optional.of(musician));
        when(this.musicianMapper.map(musician)).thenReturn(musicianResponseDTO);

        final ResponseEntity<MusicianResponseDTO> result = this.getMusicianService.getMusician("id");

        assertThat(result.getBody()).isEqualTo(musicianResponseDTO);
        assertThat(result.getStatusCode()).isEqualTo(HttpStatus.OK);

    }

    /*@Test
    void findMusicianRegistriesTest(@Random(size = 2, type = MusicianResponseDTO.class) final List<MusicianResponseDTO> musicianResponseDTOList,
                                    @Random(size = 2, type = Musician.class) final List<Musician> musicianList) {

        when(this.findMusicians.execute(FindMusiciansRequest
                .builder()
                .domain("domain")
                .subdomain("subdomain")
                .providerInput("providerInput")
                .providerOutput("providerOutput")
                .valueInput("valueInput")
                .valueOutput("valueOutput")
                .description("description")
                .build())).thenReturn(musicianList);

        when(this.musicianMapper.map(musicianList)).thenReturn(musicianResponseDTOList);

        final ResponseEntity<List<MusicianResponseDTO>> result =
                this.getMusicianService.findMusicianRegistries(
                        "domain",
                        "subdomain",
                        "providerInput",
                        "providerOutput",
                        "valueInput",
                        "valueOutput",
                        "description");

        assertThat(result.getBody()).isEqualTo(musicianResponseDTOList);
        assertThat(result.getStatusCode()).isEqualTo(HttpStatus.OK);
    }*/

    /*@Test
    void findMusicianRegistriesNoContentTest() {

        when(this.findMusicians.execute(FindMusiciansRequest
                .builder()
                .domain("domain")
                .subdomain("subdomain")
                .providerInput("providerInput")
                .providerOutput("providerOutput")
                .valueInput("valueInput")
                .valueOutput("valueOutput")
                .description("description")
                .build())).thenReturn(List.of());

        final ResponseEntity<List<MusicianResponseDTO>> result =
                this.getMusicianService.findMusicianRegistries(
                        "domain",
                        "subdomain",
                        "providerInput",
                        "providerOutput",
                        "valueInput",
                        "valueOutput",
                        "description");

        assertThat(result.getStatusCode()).isEqualTo(HttpStatus.NO_CONTENT);

    }*/
}
