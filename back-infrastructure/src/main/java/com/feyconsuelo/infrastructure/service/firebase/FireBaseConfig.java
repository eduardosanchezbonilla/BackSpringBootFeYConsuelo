package com.feyconsuelo.infrastructure.service.firebase;

import com.feyconsuelo.infrastructure.service.googledrive.GoogleDriveConfig;
import com.google.api.services.drive.DriveScopes;
import com.google.auth.oauth2.GoogleCredentials;
import com.google.firebase.FirebaseApp;
import com.google.firebase.FirebaseOptions;
import com.google.firebase.messaging.FirebaseMessaging;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.List;

@Configuration
public class FireBaseConfig {

    private static final String CREDENTIALS_FILE_PATH = "/firebase.json";
    private static final List<String> SCOPES = Collections.singletonList(DriveScopes.DRIVE);

    @Bean
    public FirebaseMessaging firebaseMessaging(final FirebaseApp firebaseApp) {
        return FirebaseMessaging.getInstance(firebaseApp);
    }

    @Bean
    public FirebaseApp firebaseApp(final GoogleCredentials credentials) {
        final FirebaseOptions options = FirebaseOptions.builder()
                .setCredentials(credentials)
                .build();

        return FirebaseApp.initializeApp(options);
    }

    @Bean
    GoogleCredentials googleCredentials() throws IOException {
        final InputStream credentialsStream = GoogleDriveConfig.class.getResourceAsStream(CREDENTIALS_FILE_PATH);

        if (credentialsStream == null) {
            throw new IOException("Resource not found: " + CREDENTIALS_FILE_PATH);
        }

        return GoogleCredentials.fromStream(credentialsStream)
                .createScoped(SCOPES);
    }

}
