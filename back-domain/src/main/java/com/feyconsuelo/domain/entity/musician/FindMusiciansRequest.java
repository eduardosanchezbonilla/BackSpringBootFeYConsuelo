package com.feyconsuelo.domain.entity.musician;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

@AllArgsConstructor
@Builder
@Data
public class FindMusiciansRequest {

    private String dni;

    private String name;

    private String surname;

    private String direction;

    private String municipality;

    private String province;

    private String voice;

}
