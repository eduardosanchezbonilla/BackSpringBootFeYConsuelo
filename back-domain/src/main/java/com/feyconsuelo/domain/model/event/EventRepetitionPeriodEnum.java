package com.feyconsuelo.domain.model.event;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

import java.time.Period;

@JsonFormat(shape = JsonFormat.Shape.OBJECT)
@Getter
public enum EventRepetitionPeriodEnum {

    SEMANAL("Semanal", 1, Period.ofWeeks(1));

    private static final EventRepetitionPeriodEnum defaultValue = SEMANAL;
    private final String id;
    private final String description;
    private final Integer order;
    private final Period period;

    EventRepetitionPeriodEnum(final String description, final Integer order, final Period period) {
        this.id = this.name();
        this.description = description;
        this.order = order;
        this.period = period;
    }

    public static EventRepetitionPeriodEnum of(final String id) {
        return EventRepetitionPeriodEnum.valueOf(id);
    }

    public static EventRepetitionPeriodEnum defaultOr(final EventRepetitionPeriodEnum id) {
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
