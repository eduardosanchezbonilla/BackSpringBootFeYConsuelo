package com.feyconsuelo.infrastructure.entities.user;

import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import lombok.Builder;
import lombok.Data;


@Data
@Builder
public class UserMusicianEntity {

    private UserEntity userEntity;
    private MusicianEntity musicianEntity;
}
