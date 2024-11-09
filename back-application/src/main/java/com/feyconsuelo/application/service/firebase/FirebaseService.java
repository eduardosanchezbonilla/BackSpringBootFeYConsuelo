package com.feyconsuelo.application.service.firebase;

public interface FirebaseService {

    void sendNotificationToTopic(final String title,
                                 final String notification,
                                 final String topic
    );

    void sendNotificationToToken(final String title,
                                 final String notification,
                                 final String token
    );

    Boolean checkToken(final String token);

}
