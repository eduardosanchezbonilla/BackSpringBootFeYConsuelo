package com.feyconsuelo.application.service.googledrive;

import com.feyconsuelo.domain.model.googledrive.FileResponse;

import java.util.List;
import java.util.Optional;

public interface GoogleDriveService {

    List<FileResponse> getAllFilesInDirectory(String directoryGoogleId);

    Optional<FileResponse> downloadFile(String fileGoogleId);

}
