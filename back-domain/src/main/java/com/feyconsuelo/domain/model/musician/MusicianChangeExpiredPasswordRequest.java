package com.feyconsuelo.domain.model.musician;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class MusicianChangeExpiredPasswordRequest {

    private String currentPassword;

    private String newPassword;

    private String repeatNewPassword;
}
