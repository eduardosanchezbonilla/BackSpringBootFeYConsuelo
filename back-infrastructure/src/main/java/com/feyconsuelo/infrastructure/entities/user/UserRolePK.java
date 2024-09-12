package com.feyconsuelo.infrastructure.entities.user;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.io.Serializable;

@Embeddable
@Data
@SuperBuilder
@NoArgsConstructor
public class UserRolePK implements Serializable {
    private static final long serialVersionUID = 1L;

    @Column(name = "username")
    private String username;

    @Column(name = "role")
    private String role;

}
