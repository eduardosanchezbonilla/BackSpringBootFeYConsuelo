package com.feyconsuelo.infrastructure.jpa.mapper;

import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.infrastructure.jpa.entities.MusicianEntity;
import org.mapstruct.InjectionStrategy;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.NullValueCheckStrategy;

import java.util.List;

@Mapper(componentModel = "spring", injectionStrategy = InjectionStrategy.CONSTRUCTOR, nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface MusicianEntityMapper {

    Musician map(MusicianEntity musicianEntity);

    // TODO, estoy asignando el usuario de modificacion a pelo, lo tendre que coger el token
    @Mapping(
            target = "modifiedUser",
            expression = "java(\"TEMPORAL\")"
    )
    MusicianEntity map(Musician musician);

    List<Musician> map(List<MusicianEntity> musicianEntities);

}
