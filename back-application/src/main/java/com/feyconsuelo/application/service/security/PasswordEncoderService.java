package com.feyconsuelo.application.service.security;

public interface PasswordEncoderService {

    String encodePassword(String password);

    Boolean matchesPassword(String password, String encodedPassword);

}
