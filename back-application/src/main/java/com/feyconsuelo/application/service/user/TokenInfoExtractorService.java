package com.feyconsuelo.application.service.user;

public interface TokenInfoExtractorService {

    String getUsername();

    Boolean hasRole(String role);

}
