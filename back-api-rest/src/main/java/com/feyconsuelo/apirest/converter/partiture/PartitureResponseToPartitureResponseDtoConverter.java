package com.feyconsuelo.apirest.converter.partiture;

import com.feyconsuelo.domain.model.partiture.PartitureResponse;
import com.feyconsuelo.openapi.model.PartitureResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class PartitureResponseToPartitureResponseDtoConverter {

    public PartitureResponseDto convert(final PartitureResponse partitureResponse) {
        return PartitureResponseDto.builder()
                .name(partitureResponse.getName())
                .googleId(partitureResponse.getGoogleId())
                .content(partitureResponse.getContent())
                .build();
    }

}
