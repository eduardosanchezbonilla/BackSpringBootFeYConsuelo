package com.feyconsuelo.domain.model.event;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

@JsonFormat(shape = JsonFormat.Shape.OBJECT)
@Getter
public enum EventTypeEnum {

    REHEARSAL("Ensayo", 1),
    PERFORMANCE("Actuacion", 2);

    private static final EventTypeEnum defaultValue = REHEARSAL;
    private final String id;
    private final String description;
    private final Integer order;

    EventTypeEnum(final String description, final Integer order) {
        this.id = this.name();
        this.description = description;
        this.order = order;
    }

    public static EventTypeEnum of(final String id) {
        return EventTypeEnum.valueOf(id);
    }

    public static EventTypeEnum defaultOr(final EventTypeEnum id) {
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
