package com.feyconsuelo.apirest.service.partiture.query;

import com.feyconsuelo.apirest.converter.partiture.PartitureResponseListToPartitureResponseDtoListConverter;
import com.feyconsuelo.domain.model.partiture.PartitureRequest;
import com.feyconsuelo.domain.model.partiture.PartitureResponse;
import com.feyconsuelo.domain.usecase.partiture.GetAllPartitures;
import com.feyconsuelo.openapi.model.PartitureResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetPartitureService {

    private final GetAllPartitures getAllPartitures;

    private final PartitureResponseListToPartitureResponseDtoListConverter partitureResponseListToPartitureResponseDtoListConverter;

    public ResponseEntity<List<PartitureResponseDto>> getAllPartituresInPartitureGroup(final PartitureRequest partitureRequest) {
        final List<PartitureResponse> partitureResponseList = this.getAllPartitures.execute(partitureRequest);
        if (CollectionUtils.isEmpty(partitureResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.partitureResponseListToPartitureResponseDtoListConverter.convert(partitureResponseList));
    }

}
