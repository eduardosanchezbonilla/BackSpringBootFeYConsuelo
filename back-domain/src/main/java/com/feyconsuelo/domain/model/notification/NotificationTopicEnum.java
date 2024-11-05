package com.feyconsuelo.domain.model.notification;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

@JsonFormat(shape = JsonFormat.Shape.OBJECT)
@Getter
public enum NotificationTopicEnum {

    GENERAL("general", "GENERAL", 3),
    ADMIN("admin", "ADMINISTRADORES", 2),
    MUSICO("musico", "MUSICOS", 1);

    private static final NotificationTopicEnum defaultValue = MUSICO;
    private final String id;
    private final String topic;
    private final String topicName;
    private final Integer order;

    NotificationTopicEnum(final String topic, final String topicName, final Integer order) {
        this.id = this.name();
        this.topic = topic;
        this.topicName = topicName;
        this.order = order;
    }

    public static NotificationTopicEnum of(final String id) {
        return NotificationTopicEnum.valueOf(id);
    }

    public static NotificationTopicEnum defaultOr(final NotificationTopicEnum id) {
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
