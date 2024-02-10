package com.feyconsuelo.apirest.mapper;

import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.openapi.model.MusicianDTO;
import com.feyconsuelo.openapi.model.MusicianResponseDTO;
import org.mapstruct.InjectionStrategy;
import org.mapstruct.Mapper;
import org.mapstruct.NullValueCheckStrategy;

import java.util.List;

@Mapper(componentModel = "spring", injectionStrategy = InjectionStrategy.CONSTRUCTOR, nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface MusicianMapper {

    Musician map(MusicianDTO musicianDTO);

    MusicianResponseDTO map(Musician musician);

    List<MusicianResponseDTO> map(List<Musician> musicians);

}
