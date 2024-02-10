package com.feyconsuelo.domain.entity.musician;

import lombok.*;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class Musician {

    private String id;

    private String dni;

    private String name;

    private String surname;

    private String direction;

    private String municipality;

    private String province;

    private String voice;

    private String image;

}
