package com.feyconsuelo.infrastructure.service.security.bcryp;

import com.feyconsuelo.application.service.security.PasswordEncoderService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class PasswordEncoderServiceImpl implements PasswordEncoderService {
    private final BCryptPasswordEncoder passwordEncoder;

    @Override
    public String encodePassword(final String rawPassword) {
        return this.passwordEncoder.encode(rawPassword);
    }

    @Override
    public Boolean matchesPassword(final String rawPassword, final String encodedPassword) {
        return this.passwordEncoder.matches(rawPassword, encodedPassword);
    }
}
