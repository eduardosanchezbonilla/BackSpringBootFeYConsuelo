package com.feyconsuelo.infrastructure.config.security;

import com.feyconsuelo.domain.model.user.UserRoleEnum;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;

@Configuration
@RequiredArgsConstructor
public class SecurityConfig {

    private final JwtRequestFilter jwtRequestFilter;

    @Bean
    public SecurityFilterChain securityFilterChain(final HttpSecurity http) throws Exception {
        http.csrf(AbstractHttpConfigurer::disable)
                .authorizeHttpRequests(
                        auth -> auth
                                .requestMatchers(new AntPathRequestMatcher("/auth/login")).permitAll()
                                .requestMatchers("/actuator/health").permitAll()
                                .requestMatchers("/").permitAll()
                                .requestMatchers("/swagger-ui.html").permitAll()
                                .requestMatchers("/swagger-ui/**").permitAll()
                                .requestMatchers("/v3/api-docs").permitAll()
                                .requestMatchers("/v3/api-docs/**").permitAll()
                                .requestMatchers("/user").hasRole(UserRoleEnum.ADMIN.getId())
                                .requestMatchers("/musician").hasAnyRole(UserRoleEnum.ADMIN.getId())
                                .requestMatchers("/voice").hasAnyRole(UserRoleEnum.ADMIN.getId())
                                .anyRequest().authenticated()
                )
                .sessionManagement(
                        session -> session
                                .sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                );

        http.addFilterBefore(this.jwtRequestFilter, UsernamePasswordAuthenticationFilter.class);

        return http.build();
    }

}