package com.feyconsuelo.infrastructure.service.security.jwt;

import com.feyconsuelo.application.service.security.JwtService;
import com.feyconsuelo.infrastructure.config.properties.JwtPropertiesConfig;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.Keys;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.crypto.SecretKey;
import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

@Service
@RequiredArgsConstructor
public class JwtServiceImpl implements JwtService {

    private final JwtPropertiesConfig jwtPropertiesConfig;

    private Claims extractAllClaims(final String token) {

        return Jwts.parser()
                .verifyWith(Keys.hmacShaKeyFor(this.jwtPropertiesConfig.getSecretKey().getBytes(StandardCharsets.UTF_8)))
                .verifyWith((SecretKey) this.getSigningKey())
                .build()
                .parseSignedClaims(token)
                .getPayload();
    }

    private <T> T extractClaim(final String token, final Function<Claims, T> claimsResolver) {
        final Claims claims = this.extractAllClaims(token);
        return claimsResolver.apply(claims);
    }

    public Date extractExpiration(final String token) {
        return this.extractClaim(token, Claims::getExpiration);
    }

    private Boolean isTokenExpired(final String token) {
        return this.extractExpiration(token).before(new Date());
    }

    private Key getSigningKey() {
        final byte[] keyBytes = this.jwtPropertiesConfig.getSecretKey().getBytes(StandardCharsets.UTF_8);
        return Keys.hmacShaKeyFor(keyBytes);
    }

    private String createToken(final Map<String, Object> claims, final String subject) {
        return Jwts.builder()
                .claims(claims)
                .subject(subject)
                .issuedAt(new Date(System.currentTimeMillis()))
                .expiration(new Date(System.currentTimeMillis() + 1000L * 60 * this.jwtPropertiesConfig.getExpirationTime()))
                .signWith(this.getSigningKey())
                .compact();
    }

    @Override
    public String extractUsername(final String token) {
        return this.extractClaim(token, Claims::getSubject);
    }

    @Override
    public String generateToken(final String username,
                                final List<String> roles
    ) {
        final Map<String, Object> claims = new HashMap<>();
        claims.put("roles",
                CollectionUtils.isEmpty(roles) ?
                        List.of() :
                        roles.stream().map(role -> "ROLE_" + role).toList()
        );
        return this.createToken(claims, username);
    }

    @Override
    public Boolean validateToken(final String token, final String username) {
        final String tokenUsername = this.extractUsername(token);
        return (tokenUsername.equals(username) && !this.isTokenExpired(token));
    }
}