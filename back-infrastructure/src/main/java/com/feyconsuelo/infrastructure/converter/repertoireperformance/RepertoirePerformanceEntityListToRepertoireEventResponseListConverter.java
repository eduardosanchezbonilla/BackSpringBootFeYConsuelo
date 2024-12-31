package com.feyconsuelo.infrastructure.converter.repertoireperformance;

import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventResponse;
import com.feyconsuelo.infrastructure.entities.repertoireperformance.RepertoirePerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoirePerformanceEntityListToRepertoireEventResponseListConverter {

    private final RepertoirePerformanceEntityToRepertoireEventResponseConverter repertoirePerformanceEntityToRepertoireEventResponseConverter;

    public List<RepertoireEventResponse> convert(final List<RepertoirePerformanceEntity> repertoirePerformanceEntityList) {
        if (CollectionUtils.isEmpty(repertoirePerformanceEntityList)) {
            return List.of();
        }
        return repertoirePerformanceEntityList.stream()
                .map(this.repertoirePerformanceEntityToRepertoireEventResponseConverter::convert)
                .toList();
    }
}
