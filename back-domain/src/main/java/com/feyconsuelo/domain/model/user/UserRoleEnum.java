package com.feyconsuelo.domain.model.user;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

@JsonFormat(shape = JsonFormat.Shape.OBJECT)
@Getter
public enum UserRoleEnum {

    ADMIN("Rol administrador"),
    MUSICO("Rol para los musicos"),
    INVITADO("Rol para usuario invitado");

    private static final UserRoleEnum defaultValue = MUSICO;
    private final String id;
    private final String description;

    UserRoleEnum(final String description) {
        this.id = this.name();
        this.description = description;
    }

    public static UserRoleEnum of(final String id) {
        return UserRoleEnum.valueOf(id);
    }

    public static UserRoleEnum defaultOr(final UserRoleEnum id) {
        return id != null ? id : defaultValue;
    }

    @Override
    public String toString() {
        return this.id;
    }

    @JsonValue
    public String getId() {
        return this.id;
    }

}
