package com.feyconsuelo.domain.model.musician;

import com.feyconsuelo.domain.model.voice.VoiceResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class MusicianResponse {

    private Long id;

    private String dni;

    private String name;

    private String surname;

    private String direction;

    private String municipality;

    private String province;

    private String email;

    private VoiceResponse voice;

    private String image;

    private LocalDateTime deleteDate;

    private LocalDateTime birthDate;

    private LocalDateTime registrationDate;

    private String inventoryObservations;

    private Long idLastRehearsal;

    private Boolean assistLastRehearsal;

    private Boolean assistBus;

    private LocalDate dateLastRehearsal;


}

