package com.feyconsuelo.domain.model.notification;

import com.feyconsuelo.domain.model.voice.VoiceResponse;
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
public class NotificationUserTokenResponse {

    private String username;

    private String name;

    private String email;

    private String role;

    private VoiceResponse voice;

    private List<String> tokens;

}