package com.feyconsuelo.infrastructure.config.security;

import com.feyconsuelo.infrastructure.service.security.jwt.JwtServiceImpl;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Component
@RequiredArgsConstructor
public class JwtRequestFilter extends OncePerRequestFilter {

    private static final String AUTHORIZATION_HEADER = "Authorization";
    private static final String BEARER_HEADER = "Bearer ";

    private final UserDetailsService userDetailsService;

    private final JwtServiceImpl jwtService;

    @Override
    protected void doFilterInternal(final HttpServletRequest request,
                                    final HttpServletResponse response,
                                    final FilterChain chain
    ) throws ServletException, IOException {

        final String authorizationHeader = request.getHeader(AUTHORIZATION_HEADER);

        // pongo la condicion de que venga bearer pq para el de login, no mando token, y por tanto no tengo que validar nadapara el resto si
        // si no me enviaran bearer, como no tienen permitAll, tampoco se validarian bien
        if (Boolean.FALSE.equals(StringUtils.isEmpty(authorizationHeader)) && authorizationHeader.startsWith(BEARER_HEADER)) {
            final String jwt = authorizationHeader.substring(7);
            final String username = this.jwtService.extractUsername(jwt);

            if (username != null && SecurityContextHolder.getContext().getAuthentication() == null) {

                final UserDetails userDetails = this.userDetailsService.loadUserByUsername(username);

                if (Boolean.TRUE.equals(this.jwtService.validateToken(jwt, userDetails.getUsername()))) {
                    final UsernamePasswordAuthenticationToken usernamePasswordAuthenticationToken = new UsernamePasswordAuthenticationToken(userDetails, null, userDetails.getAuthorities());
                    usernamePasswordAuthenticationToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                    SecurityContextHolder.getContext().setAuthentication(usernamePasswordAuthenticationToken);
                    this.refreshToken(jwt, username, response);
                }
            }
        }
        chain.doFilter(request, response);
    }

    private void refreshToken(final String token,
                              final String username,
                              final HttpServletResponse response
    ) {
        final Integer maxExpirationTime = this.jwtService.getMaxExpirationTime() * 60; // en segundos
        final long tokenExpirationTime = (this.jwtService.getExpirationTime(token) - System.currentTimeMillis()) / 1000; // en segundos

        final long renewalThreshold = maxExpirationTime * 30 / 100; // 30% of the token expiration time
        if (tokenExpirationTime < renewalThreshold) {
            response.setHeader(AUTHORIZATION_HEADER, BEARER_HEADER + this.jwtService.generateToken(username, this.jwtService.getRoles(token)));
        } else {
            response.setHeader(AUTHORIZATION_HEADER, BEARER_HEADER + token);
        }
    }
}