package com.feyconsuelo.infrastructure.converter.repertoirerehearsal;

import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventResponse;
import com.feyconsuelo.infrastructure.entities.repertoirerehearsal.RepertoireRehearsalEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireRehearsalEntityListToRepertoireEventResponseListConverter {

    private final RepertoireRehearsalEntityToRepertoireEventResponseConverter repertoireRehearsalEntityToRepertoireEventResponseConverter;

    public List<RepertoireEventResponse> convert(final List<RepertoireRehearsalEntity> repertoireRehearsalEntityList) {
        if (CollectionUtils.isEmpty(repertoireRehearsalEntityList)) {
            return List.of();
        }
        return repertoireRehearsalEntityList.stream()
                .map(this.repertoireRehearsalEntityToRepertoireEventResponseConverter::convert)
                .toList();
    }
}
