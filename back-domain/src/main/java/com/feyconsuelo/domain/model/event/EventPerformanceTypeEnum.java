package com.feyconsuelo.domain.model.event;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

@JsonFormat(shape = JsonFormat.Shape.OBJECT)
@Getter
public enum EventPerformanceTypeEnum {

    CONCIERTO("Concierto", 1),
    DESFILE_PROCESIONAL("Desfile procesional", 2);

    private static final EventPerformanceTypeEnum defaultValue = DESFILE_PROCESIONAL;
    private final String id;
    private final String description;
    private final Integer order;

    EventPerformanceTypeEnum(final String description, final Integer order) {
        this.id = this.name();
        this.description = description;
        this.order = order;
    }

    public static EventPerformanceTypeEnum of(final String id) {
        return EventPerformanceTypeEnum.valueOf(id);
    }

    public static EventPerformanceTypeEnum defaultOr(final EventPerformanceTypeEnum id) {
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
