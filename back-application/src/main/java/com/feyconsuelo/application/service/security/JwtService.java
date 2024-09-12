package com.feyconsuelo.application.service.security;

import java.util.List;

public interface JwtService {

    String generateToken(final String username,
                         final List<String> roles);

    String extractUsername(final String token);

    Boolean validateToken(final String token, final String username);
}
