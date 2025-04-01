package com.feyconsuelo.domain.model.auth;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.musicianmarchsolo.MusicianMarchSoloResponse;
import com.feyconsuelo.domain.model.user.UserResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class AuthResponse {

    String username;
    List<String> roles;
    String token;
    MusicianResponse musician;
    List<MusicianMarchSoloResponse> musicianMarchSolos;
    UserResponse user;
    List<EventResponse> todayPerformance;

}
