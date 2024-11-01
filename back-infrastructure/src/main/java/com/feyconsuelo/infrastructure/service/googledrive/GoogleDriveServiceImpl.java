package com.feyconsuelo.infrastructure.service.googledrive;

import com.feyconsuelo.application.service.googledrive.GoogleDriveService;
import com.feyconsuelo.application.service.utils.StringService;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.googledrive.FileResponse;
import com.feyconsuelo.infrastructure.converter.googledrive.FileListToFileResponseListConverter;
import com.google.api.client.http.HttpResponseException;
import com.google.api.services.drive.Drive;
import com.google.api.services.drive.model.File;
import com.google.api.services.drive.model.FileList;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
@Slf4j
@RequiredArgsConstructor
public class GoogleDriveServiceImpl implements GoogleDriveService {

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
            service.files().get(fileGoogleId).executeMediaAndDownloadTo(outputStream);
            return Optional.of(
                    FileResponse.builder()
                            .googleId(fileGoogleId)
                            .content(this.stringService.convertToBase64(outputStream.toByteArray()))
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


}
