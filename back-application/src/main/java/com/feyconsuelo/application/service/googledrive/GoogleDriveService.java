package com.feyconsuelo.application.service.googledrive;

import com.feyconsuelo.domain.model.googledrive.FileResponse;

import java.util.List;
import java.util.Optional;

public interface GoogleDriveService {

    List<FileResponse> getAllFilesInDirectory(String directoryGoogleId);

    Optional<FileResponse> downloadFile(String fileGoogleId);

    Optional<FileResponse> uploadFile(final String folderGoogleId, final String name, final String base64Content, final String mimeType);

    void deleteFile(final String fileGoogleId);

    Optional<FileResponse> createFolder(final String folderName, final String parentFolderGoogleId);

    Optional<FileResponse> renameFolder(final String folderGoogleId, final String newName);

    void deleteFolderRecursively(final String folderGoogleId);
}
