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

    /*
    public static String getfiles() throws IOException, GeneralSecurityException {

        final Drive service = getDriveService();

        // Print the names and IDs for up to 10 files.
        final FileList result = service.files().list()
                .setPageSize(10)
                .execute();
        final List<File> files = result.getFiles();
        if (files == null || files.isEmpty()) {
            System.out.println("No files found.");
            return "No files found.";
        } else {
            return files.toString();
        }
    }

    public static void downloadFile(final String fileId, final String destinationPath) throws IOException, GeneralSecurityException {
        final Drive service = getDriveService();
        final OutputStream outputStream = new FileOutputStream(destinationPath);
        service.files().get(fileId).executeMediaAndDownloadTo(outputStream);
    }

    public static byte[] downloadFileToMemory(final String fileId) throws IOException, GeneralSecurityException {
        final Drive service = getDriveService();
        final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        service.files().get(fileId).executeMediaAndDownloadTo(outputStream);
        return outputStream.toByteArray();
    }

    public static String convertToBase64(final byte[] fileData) {
        // Convertir el array de bytes a Base64
        return Base64.getEncoder().encodeToString(fileData);
    }

    public static void listFilesInFolder(final String folderId) throws IOException, GeneralSecurityException {
        // Obtén el servicio de Google Drive
        final Drive service = getDriveService();

        // Construye la query para listar archivos dentro de una carpeta específica
        final String query = "'" + folderId + "' in parents";

        // Realiza la búsqueda de archivos en la carpeta
        final FileList result = service.files().list()
                .setQ(query) // Aplicar la consulta para buscar por carpeta
                .setPageSize(100) // Configura el tamaño de la página según tu necesidad
                .setFields("nextPageToken, files(id, name)") // Solo obtenemos el ID y nombre de los archivos
                .execute();

        // Obtener la lista de archivos
        final List<File> files = result.getFiles();
        if (files == null || files.isEmpty()) {
            System.out.println("No files found in folder.");
        } else {
            System.out.println("Files in folder:");
            for (final File file : files) {
                System.out.printf("File: %s (ID: %s)\n", file.getName(), file.getId());
            }
        }
    }

    public static void main(final String[] args) {
        try {
            // 1P2wq4_si7sdOgbV4wa7k1j_BEbmK_47s
            //downloadFile("1cjeVLWLbUw34njuEGo9QwL4iCZoksfLk", "./prueba.pdf");
            final byte[] partiture = downloadFileToMemory("1cjeVLWLbUw34njuEGo9QwL4iCZoksfLk");
            System.out.println(convertToBase64(partiture));
            // quiero medir el tiempo de ejecucion de la siguiente instruccion

            final long startTime = System.currentTimeMillis();

            listFilesInFolder("1wSJLT0x0ckXzkE9Y7QZbPtnYWX-4kvqA");

            listFilesInFolder("1wPcGQFz9azcRgOkVUqJuHtzIF9JJcEDJ");

            listFilesInFolder("1wOwoc2NypcX2kIknbKf3FXN7dKV9p8vG");

            final long endTime = System.currentTimeMillis();
            final long elapsedTime = endTime - startTime;

            System.out.println("Execution time: " + elapsedTime + " milliseconds");
            // System.out.println(getfiles());
        } catch (final IOException | GeneralSecurityException e) {
            e.printStackTrace();
        }
    }*/

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
