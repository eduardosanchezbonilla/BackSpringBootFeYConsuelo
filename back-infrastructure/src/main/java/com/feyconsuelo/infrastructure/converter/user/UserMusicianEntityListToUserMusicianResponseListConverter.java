package com.feyconsuelo.infrastructure.converter.user;

import com.feyconsuelo.domain.model.user.UserMusicianResponse;
import com.feyconsuelo.infrastructure.entities.user.UserMusicianEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserMusicianEntityListToUserMusicianResponseListConverter {

    private final UserMusicianEntityToUserMusicianResponseConverter userMusicianEntityToUserMusicianResponseConverter;

    public List<UserMusicianResponse> convert(final List<UserMusicianEntity> userMusicianEntityList) {
        if (CollectionUtils.isEmpty(userMusicianEntityList)) {
            return List.of();
        }
        return userMusicianEntityList.stream()
                .map(this.userMusicianEntityToUserMusicianResponseConverter::convert)
                .toList();
    }
}
