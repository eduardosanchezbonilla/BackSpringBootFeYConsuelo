package com.feyconsuelo.infrastructure.service.googledrive;

import com.feyconsuelo.application.service.googledrive.GoogleDriveService;
import com.feyconsuelo.application.service.utils.StringService;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.googledrive.FileResponse;
import com.feyconsuelo.infrastructure.converter.googledrive.FileListToFileResponseListConverter;
import com.google.api.client.http.ByteArrayContent;
import com.google.api.client.http.HttpResponseException;
import com.google.api.services.drive.Drive;
import com.google.api.services.drive.model.File;
import com.google.api.services.drive.model.FileList;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Service
@Slf4j
@RequiredArgsConstructor
public class GoogleDriveServiceImpl implements GoogleDriveService {

    private static final String MIME_TYPE_FOLDER = "application/vnd.google-apps.folder";
    private final FileListToFileResponseListConverter fileListToFileResponseListConverter;
    private final GoogleDriveConfig googleDriveConfig;
    private final StringService stringService;

    @Override
    public List<FileResponse> getAllFilesInDirectory(final String folderId) {
        final List<File> responseFiles = new ArrayList<>();
        try {
            final Drive service = this.googleDriveConfig.getDriveService();

            // Construye la query para listar archivos dentro de una carpeta específica
            final String query = "'" + folderId + "' in parents";

            String nextPageToken = null;

            // Bucle para manejar la paginación
            do {
                // Realiza la búsqueda de archivos en la carpeta
                final FileList result = service.files().list()
                        .setQ(query) // Aplicar la consulta para buscar por carpeta
                        .setPageSize(100) // Configura el tamaño de la página según tu necesidad
                        .setFields("nextPageToken, files(id, name)") // Solo obtenemos el ID y nombre de los archivos
                        .setPageToken(nextPageToken) // Establece el token para obtener la siguiente página de resultados
                        .execute();

                // Obtener la lista de archivos de esta página
                final List<File> files = result.getFiles();

                if (files != null && !files.isEmpty()) {
                    responseFiles.addAll(files);
                }

                // Obtener el token de la siguiente página (si existe)
                nextPageToken = result.getNextPageToken();

            } while (nextPageToken != null); // Continua si hay más páginas

            return this.fileListToFileResponseListConverter.convert(responseFiles);
        } catch (final HttpResponseException hre) {
            if (hre.getStatusCode() == 404) {
                throw new NotFoundException("No existe la carpeta en Google Drive");
            } else {
                throw new FeYConsueloException("Error al obtener los archivos de Google Drive", hre);
            }
        } catch (final Exception e) {
            throw new FeYConsueloException("Error al obtener los archivos de Google Drive", e);
        }
    }

    @Override
    public Optional<FileResponse> downloadFile(final String fileGoogleId) {
        try {
            final Drive service = this.googleDriveConfig.getDriveService();
            final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            final File fileMetadata = service.files().get(fileGoogleId).setFields("mimeType, name").execute();
            final String mimeType = fileMetadata.getMimeType();
            service.files().get(fileGoogleId).executeMediaAndDownloadTo(outputStream);
            return Optional.of(
                    FileResponse.builder()
                            .googleId(fileGoogleId)
                            .content(this.stringService.convertToBase64(outputStream.toByteArray()))
                            .mimeType(mimeType)
                            .build()
            );
        } catch (final HttpResponseException hre) {
            if (hre.getStatusCode() == 404) {
                throw new NotFoundException("Archivo no encontrado en Google Drive");
            } else {
                throw new FeYConsueloException("Error al descargar el archivo de Google Drive", hre);
            }
        } catch (final Exception e) {
            throw new FeYConsueloException("Error al descargar el archivo de Google Drive", e);
        }
    }

    public Optional<FileResponse> uploadFile(final String folderGoogleId, final String name, final String base64Content, final String mimeType) {
        try {
            final Drive service = this.googleDriveConfig.getDriveService();
            final byte[] fileBytes = this.stringService.convertToBytes(base64Content);
            final ByteArrayContent contentStream = new ByteArrayContent(mimeType, fileBytes);
            final com.google.api.services.drive.model.File fileMetadata = new com.google.api.services.drive.model.File();
            fileMetadata.setName(name);
            fileMetadata.setParents(Collections.singletonList(folderGoogleId));

            // Subimos el fichero y solicitamos los campos que queremos que se devuelvan (id, name, mimeType)
            final com.google.api.services.drive.model.File uploadedFile = service.files().create(fileMetadata, contentStream)
                    .setFields("id, name, mimeType")
                    .execute();

            return Optional.of(
                    FileResponse.builder()
                            .googleId(uploadedFile.getId())
                            .content(base64Content)
                            .mimeType(uploadedFile.getMimeType())
                            .build()
            );
        } catch (final Exception e) {
            throw new FeYConsueloException("Error al subir el archivo a Google Drive", e);
        }
    }

    public void deleteFile(final String fileGoogleId) {
        try {
            final Drive service = this.googleDriveConfig.getDriveService();
            service.files().delete(fileGoogleId).execute();
        } catch (final HttpResponseException hre) {
            if (hre.getStatusCode() == 404) {
                throw new NotFoundException("Archivo no encontrado en Google Drive");
            } else {
                throw new FeYConsueloException("Error al eliminar el archivo de Google Drive", hre);
            }
        } catch (final Exception e) {
            throw new FeYConsueloException("Error al eliminar el archivo de Google Drive", e);
        }
    }

    public Optional<FileResponse> createFolder(final String folderName, final String parentFolderGoogleId) {
        try {
            final Drive service = this.googleDriveConfig.getDriveService();
            final com.google.api.services.drive.model.File folderMetadata = new com.google.api.services.drive.model.File();
            folderMetadata.setName(folderName);
            folderMetadata.setMimeType(MIME_TYPE_FOLDER);
            folderMetadata.setParents(Collections.singletonList(parentFolderGoogleId));

            // Creamos la carpeta y solicitamos que se devuelvan algunos campos
            final com.google.api.services.drive.model.File folder = service.files().create(folderMetadata)
                    .setFields("id, name")
                    .execute();

            // Retornamos la respuesta con los datos de la carpeta creada
            return Optional.of(
                    FileResponse.builder()
                            .googleId(folder.getId())
                            .content(null)  // No hay contenido para una carpeta
                            .mimeType(MIME_TYPE_FOLDER)
                            .build()
            );
        } catch (final Exception e) {
            throw new FeYConsueloException("Error al crear la carpeta en Google Drive", e);
        }
    }

    public Optional<FileResponse> renameFolder(final String folderGoogleId, final String newName) {
        try {
            final Drive service = this.googleDriveConfig.getDriveService();
            final com.google.api.services.drive.model.File updatedMetadata = new com.google.api.services.drive.model.File();
            updatedMetadata.setName(newName);

            // Realizamos la actualización únicamente del nombre de la carpeta
            final com.google.api.services.drive.model.File updatedFolder = service.files().update(folderGoogleId, updatedMetadata)
                    .setFields("id, name, mimeType")
                    .execute();

            return Optional.of(
                    FileResponse.builder()
                            .googleId(updatedFolder.getId())
                            .content(null)  // No hay contenido, ya que es una carpeta
                            .mimeType(updatedFolder.getMimeType())
                            .build()
            );
        } catch (final HttpResponseException hre) {
            if (hre.getStatusCode() == 404) {
                throw new NotFoundException("Carpeta no encontrada en Google Drive");
            } else {
                throw new FeYConsueloException("Error al renombrar la carpeta en Google Drive", hre);
            }
        } catch (final Exception e) {
            throw new FeYConsueloException("Error al renombrar la carpeta en Google Drive", e);
        }
    }

    public void deleteFolderRecursively(final String folderGoogleId) {
        try {
            final Drive service = this.googleDriveConfig.getDriveService();

            // Construimos una consulta para obtener todos los archivos y carpetas
            // que tengan como padre la carpeta indicada.
            final String query = "'" + folderGoogleId + "' in parents";
            final com.google.api.services.drive.model.FileList fileList = service.files().list()
                    .setQ(query)
                    .setFields("files(id, mimeType)")
                    .execute();

            // Recorremos cada elemento contenido en la carpeta
            for (final com.google.api.services.drive.model.File file : fileList.getFiles()) {
                // Si es una carpeta, se llama recursivamente para eliminar su contenido
                if (MIME_TYPE_FOLDER.equals(file.getMimeType())) {
                    this.deleteFolderRecursively(file.getId());
                } else {
                    // Si es un archivo, se elimina directamente
                    service.files().delete(file.getId()).execute();
                }
            }

            // Finalmente, se elimina la carpeta principal
            service.files().delete(folderGoogleId).execute();
        } catch (final HttpResponseException hre) {
            if (hre.getStatusCode() == 404) {
                throw new NotFoundException("Carpeta no encontrada en Google Drive");
            } else {
                throw new FeYConsueloException("Error al eliminar la carpeta en Google Drive", hre);
            }
        } catch (final Exception e) {
            throw new FeYConsueloException("Error al eliminar la carpeta en Google Drive", e);
        }
    }


}
