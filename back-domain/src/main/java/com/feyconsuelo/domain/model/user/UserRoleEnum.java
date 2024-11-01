package com.feyconsuelo.domain.model.user;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

@JsonFormat(shape = JsonFormat.Shape.OBJECT)
@Getter
public enum UserRoleEnum {

    SUPER_ADMIN("Rol super administrador", "Super Administrador", 1),
    ADMIN("Rol administrador", "Administrador", 2),
    MUSICO("Rol para los musicos", "Musico", 3),
    INVITADO("Rol para usuario invitado", "Invitado", 4);

    private static final UserRoleEnum defaultValue = MUSICO;
    private final String id;
    private final String description;
    private final String roleName;
    private final Integer order;

    UserRoleEnum(final String description, final String roleName, final Integer order) {
        this.id = this.name();
        this.description = description;
        this.roleName = roleName;
        this.order = order;
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
