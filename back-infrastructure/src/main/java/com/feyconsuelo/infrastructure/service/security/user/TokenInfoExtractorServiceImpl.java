package com.feyconsuelo.infrastructure.service.security.user;

import com.feyconsuelo.application.service.user.TokenInfoExtractorService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class TokenInfoExtractorServiceImpl implements TokenInfoExtractorService {

    @Override
    public String getUsername() {
        final Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.isAuthenticated()) {
            final Object principal = authentication.getPrincipal();
            if (principal instanceof final UserDetails userDetails) {
                return userDetails.getUsername();
            }
        }
        return "NOT_USER";
    }

    @Override
    public Boolean hasRole(final String role) {
        final Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.isAuthenticated()) {
            final Object principal = authentication.getPrincipal();
            if (principal instanceof final UserDetails userDetails) {
                return userDetails.getAuthorities().stream()
                        .anyMatch(authority -> authority.getAuthority().equals("ROLE_" + role));
            }
        }
        return Boolean.FALSE;
    }

}
