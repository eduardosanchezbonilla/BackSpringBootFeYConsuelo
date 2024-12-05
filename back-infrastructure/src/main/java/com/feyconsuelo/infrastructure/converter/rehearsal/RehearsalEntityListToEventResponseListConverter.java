package com.feyconsuelo.infrastructure.converter.rehearsal;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RehearsalEntityListToEventResponseListConverter {

    private final RehearsalEntityToEventResponseConverter rehearsalEntityToEventResponseConverter;

    public List<EventResponse> convert(final List<RehearsalEntity> rehearsalEntityList) {
        if (CollectionUtils.isEmpty(rehearsalEntityList)) {
            return List.of();
        }
        return rehearsalEntityList.stream()
                .map(this.rehearsalEntityToEventResponseConverter::convert)
                .toList();
    }
}
