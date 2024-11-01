package com.feyconsuelo.domain.model.user;

import com.feyconsuelo.domain.model.musician.MusicianResponse;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class UserMusicianResponse {

    private UserResponse userResponse;
    private MusicianResponse musicianResponse;

}
