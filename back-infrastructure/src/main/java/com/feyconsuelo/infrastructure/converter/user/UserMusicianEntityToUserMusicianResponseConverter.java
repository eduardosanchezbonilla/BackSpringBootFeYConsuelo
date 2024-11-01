package com.feyconsuelo.infrastructure.converter.user;

import com.feyconsuelo.domain.model.user.UserMusicianResponse;
import com.feyconsuelo.infrastructure.converter.musician.MusicianEntityToMusicianResponseConverter;
import com.feyconsuelo.infrastructure.entities.user.UserMusicianEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserMusicianEntityToUserMusicianResponseConverter {

    private final UserEntityToUserResponseConverter userEntityToUserResponseConverter;
    private final MusicianEntityToMusicianResponseConverter musicianEntityToMusicianResponseConverter;

    public UserMusicianResponse convert(final UserMusicianEntity userMusicianEntity) {
        return UserMusicianResponse.builder()
                .userResponse(this.userEntityToUserResponseConverter.convert(userMusicianEntity.getUserEntity()))
                .musicianResponse(userMusicianEntity.getMusicianEntity() == null ? null : this.musicianEntityToMusicianResponseConverter.convert(userMusicianEntity.getMusicianEntity()))
                .build();
    }

}
