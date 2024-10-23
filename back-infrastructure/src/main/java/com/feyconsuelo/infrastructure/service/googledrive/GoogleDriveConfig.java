package com.feyconsuelo.infrastructure.service.googledrive;

import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.json.JsonFactory;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.drive.Drive;
import com.google.api.services.drive.DriveScopes;
import com.google.auth.oauth2.GoogleCredentials;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.InputStream;
import java.security.GeneralSecurityException;
import java.util.Collections;
import java.util.List;

@Service
public class GoogleDriveConfig {

    private static final String APPLICATION_NAME = "BackSpringBootFeYConsuelo";
    private static final JsonFactory JSON_FACTORY = JacksonFactory.getDefaultInstance();
    private static final List<String> SCOPES = Collections.singletonList(DriveScopes.DRIVE);
    private static final String CREDENTIALS_FILE_PATH = "/credentials.json";

    public Drive getDriveService() throws IOException, GeneralSecurityException {
        final InputStream credentialsStream = GoogleDriveConfig.class.getResourceAsStream(CREDENTIALS_FILE_PATH);

        if (credentialsStream == null) {
            throw new IOException("Resource not found: " + CREDENTIALS_FILE_PATH);
        }

        final GoogleCredentials credentials = GoogleCredentials.fromStream(credentialsStream)
                .createScoped(SCOPES);

        return new Drive.Builder(GoogleNetHttpTransport.newTrustedTransport(), JSON_FACTORY, new com.google.auth.http.HttpCredentialsAdapter(credentials))
                .setApplicationName(APPLICATION_NAME)
                .build();
    }

    // TODO
    // hay que compartir a este usuario:
    // backspringbootfeyconsuelo@compact-market-437808-e7.iam.gserviceaccount.com

}
