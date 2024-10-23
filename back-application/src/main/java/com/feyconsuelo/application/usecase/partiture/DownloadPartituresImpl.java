package com.feyconsuelo.application.usecase.partiture;

import com.feyconsuelo.application.converter.googledrive.FileResponseToPartitureResponseConverter;
import com.feyconsuelo.application.service.googledrive.GoogleDriveService;
import com.feyconsuelo.domain.model.googledrive.FileResponse;
import com.feyconsuelo.domain.model.partiture.PartitureResponse;
import com.feyconsuelo.domain.usecase.partiture.DownloadPartiture;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class DownloadPartituresImpl implements DownloadPartiture {

    private final GoogleDriveService googleDriveService;
    private final FileResponseToPartitureResponseConverter fileResponseToPartitureResponseConverter;

    @Override
    public Optional<PartitureResponse> execute(final String partitureGoogleId) {
        final Optional<FileResponse> file = this.googleDriveService.downloadFile(partitureGoogleId);
        return file.map(this.fileResponseToPartitureResponseConverter::convert);
    }
}
