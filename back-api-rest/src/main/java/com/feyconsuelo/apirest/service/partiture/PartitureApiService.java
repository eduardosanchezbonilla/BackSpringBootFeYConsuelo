package com.feyconsuelo.apirest.service.partiture;

import com.feyconsuelo.apirest.service.partiture.query.DownloadPartitureService;
import com.feyconsuelo.apirest.service.partiture.query.GetPartitureService;
import com.feyconsuelo.domain.model.partiture.PartitureRequest;
import com.feyconsuelo.openapi.api.PartitureControllerApiDelegate;
import com.feyconsuelo.openapi.model.PartitureResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class PartitureApiService implements PartitureControllerApiDelegate {

    private final GetPartitureService getPartitureGroup;
    private final DownloadPartitureService downloadPartitureService;


    @Override
    public ResponseEntity<List<PartitureResponseDto>> getAllPartituresInPartitureGroup(final String partitureGroupGoogleId) {
        return this.getPartitureGroup.getAllPartituresInPartitureGroup(
                PartitureRequest.builder()
                        .partitureGroupGoogleId(partitureGroupGoogleId)
                        .build()
        );
    }

    @Override
    public ResponseEntity<PartitureResponseDto> downloadPartiture(final String partitureGoogleId) {
        return this.downloadPartitureService.downloadPartiture(partitureGoogleId);
    }


}
