package com.feyconsuelo.apirest.converter.statistics;

import com.feyconsuelo.domain.model.statistics.RepertoireMarchEventStatisticResponse;
import com.feyconsuelo.openapi.model.RepertoireMarchEventStatisticResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchEventStatisticResponseToRepertoireMarchEventStatisticResponseDtoConverter {

    public List<RepertoireMarchEventStatisticResponseDto> convert(final List<RepertoireMarchEventStatisticResponse> repertoireMarchEventStatistic) {
        if (CollectionUtils.isEmpty(repertoireMarchEventStatistic)) {
            return List.of();
        }
        return repertoireMarchEventStatistic.stream()
                .map(musicianInfo -> RepertoireMarchEventStatisticResponseDto.builder()
                        .categoryId(musicianInfo.getCategoryId())
                        .categoryName(musicianInfo.getCategoryName())
                        .typeId(musicianInfo.getTypeId())
                        .typeName(musicianInfo.getTypeName())
                        .typeImage(musicianInfo.getTypeImage())
                        .typeOrder(musicianInfo.getTypeOrder())
                        .marchId(musicianInfo.getMarchId())
                        .marchName(musicianInfo.getMarchName())
                        .marchAuthor(musicianInfo.getMarchAuthor())
                        .quantity(musicianInfo.getQuantity())
                        .percentage(musicianInfo.getPercentage())
                        .maxQuantity(musicianInfo.getMaxQuantity())
                        .build()
                )
                .sorted(
                        Comparator
                                .comparing(RepertoireMarchEventStatisticResponseDto::getTypeOrder)
                                .thenComparing(
                                        Comparator.comparing(RepertoireMarchEventStatisticResponseDto::getPercentage).reversed()
                                )
                )
                .toList();

    }

}
