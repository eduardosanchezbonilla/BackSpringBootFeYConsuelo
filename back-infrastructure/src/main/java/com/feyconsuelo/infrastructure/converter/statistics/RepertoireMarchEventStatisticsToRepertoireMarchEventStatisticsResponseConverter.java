package com.feyconsuelo.infrastructure.converter.statistics;

import com.feyconsuelo.domain.model.statistics.RepertoireMarchEventStatisticResponse;
import com.feyconsuelo.infrastructure.entities.statistics.RepertoireMarchEventStatistics;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchEventStatisticsToRepertoireMarchEventStatisticsResponseConverter {

    public List<RepertoireMarchEventStatisticResponse> convert(final List<RepertoireMarchEventStatistics> entityList) {
        if (CollectionUtils.isEmpty(entityList)) {
            return List.of();
        } else {
            return entityList.stream()
                    .map(entity ->
                            RepertoireMarchEventStatisticResponse.builder()
                                    .categoryId(entity.getCategoryId())
                                    .categoryName(entity.getCategoryName())
                                    .typeId(entity.getTypeId())
                                    .typeName(entity.getTypeName())
                                    .typeImage(entity.getTypeImage())
                                    .typeOrder(entity.getTypeOrder())
                                    .marchId(entity.getMarchId())
                                    .marchName(entity.getMarchName())
                                    .marchAuthor(entity.getMarchAuthor())
                                    .quantity(entity.getQuantity())
                                    .percentage(entity.getPercentage())
                                    .maxQuantity(entity.getMaxQuantity())
                                    .build()
                    )
                    .toList();
        }
    }

}
