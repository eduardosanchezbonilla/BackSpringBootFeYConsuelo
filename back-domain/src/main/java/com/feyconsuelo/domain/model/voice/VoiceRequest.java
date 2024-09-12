package com.feyconsuelo.domain.model.voice;

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
public class VoiceRequest {

    private Integer order;

    private String name;

    private String image;

}
