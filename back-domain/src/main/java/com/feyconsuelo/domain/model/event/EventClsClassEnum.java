package com.feyconsuelo.domain.model.event;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

@JsonFormat(shape = JsonFormat.Shape.OBJECT)
@Getter
public enum EventClsClassEnum {

    ACTUACION_DAY("actuacion-day", 1),
    ENSAYO_GENERAL_DAY("ensayo-general-day", 2),
    ACTUACION_DAY_OK("actuacion-day-ok", 3),
    ENSAYO_GENERAL_DAY_OK("ensayo-general-day-ok", 4),
    ACTUACION_DAY_OK_KO("actuacion-day-ok-ko", 5);

    private static final EventClsClassEnum defaultValue = ENSAYO_GENERAL_DAY;
    private final String id;
    private final String description;
    private final Integer order;

    EventClsClassEnum(final String description, final Integer order) {
        this.id = this.name();
        this.description = description;
        this.order = order;
    }

    public static EventClsClassEnum of(final String id) {
        return EventClsClassEnum.valueOf(id);
    }

    public static EventClsClassEnum defaultOr(final EventClsClassEnum id) {
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
