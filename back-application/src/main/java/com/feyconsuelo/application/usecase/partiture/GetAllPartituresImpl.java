package com.feyconsuelo.application.usecase.partiture;

import com.feyconsuelo.application.converter.googledrive.FileResponseListToPartitureResponseListConverter;
import com.feyconsuelo.application.service.googledrive.GoogleDriveService;
import com.feyconsuelo.domain.model.googledrive.FileResponse;
import com.feyconsuelo.domain.model.partiture.PartitureRequest;
import com.feyconsuelo.domain.model.partiture.PartitureResponse;
import com.feyconsuelo.domain.usecase.partiture.GetAllPartitures;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetAllPartituresImpl implements GetAllPartitures {

    private final GoogleDriveService googleDriveService;
    private final FileResponseListToPartitureResponseListConverter fileResponseListToPartitureResponseListConverter;

    @Override
    public List<PartitureResponse> execute(final PartitureRequest partitureRequest) {
        final List<FileResponse> files = this.googleDriveService.getAllFilesInDirectory(partitureRequest.getPartitureGroupGoogleId());

        return this.fileResponseListToPartitureResponseListConverter.convert(files);
    }
}
