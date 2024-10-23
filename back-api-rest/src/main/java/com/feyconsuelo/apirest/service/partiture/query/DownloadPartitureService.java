package com.feyconsuelo.apirest.service.partiture.query;

import com.feyconsuelo.apirest.converter.partiture.PartitureResponseToPartitureResponseDtoConverter;
import com.feyconsuelo.domain.model.partiture.PartitureResponse;
import com.feyconsuelo.domain.usecase.partiture.DownloadPartiture;
import com.feyconsuelo.openapi.model.PartitureResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class DownloadPartitureService {

    private final DownloadPartiture downloadPartiture;

    private final PartitureResponseToPartitureResponseDtoConverter partitureResponseToPartitureResponseDtoConverter;

    public ResponseEntity<PartitureResponseDto> downloadPartiture(final String partitureGoogleId) {
        final Optional<PartitureResponse> partitureResponse = this.downloadPartiture.execute(partitureGoogleId);

        return partitureResponse.map(response -> ResponseEntity.ok(this.partitureResponseToPartitureResponseDtoConverter.convert(response))).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
