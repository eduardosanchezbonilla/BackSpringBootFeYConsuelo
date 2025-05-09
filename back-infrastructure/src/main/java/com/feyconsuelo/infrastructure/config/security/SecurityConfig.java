package com.feyconsuelo.infrastructure.config.security;

import com.feyconsuelo.domain.model.user.UserRoleEnum;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;

@Configuration
@RequiredArgsConstructor
public class SecurityConfig {

    private static final String ENDPOINT_REPERTOIRE_CATEGORY = "repertoire-category";
    private static final String ENDPOINT_REPERTOIRE = "repertoire";
    private static final String ENDPOINT_REPERTOIRE_MARCH = "repertoire-march";
    private static final String ENDPOINT_REPERTOIRE_MARCH_TYPE = "repertoire-march-type";

    private final JwtRequestFilter jwtRequestFilter;

    @Bean
    public SecurityFilterChain securityFilterChain(final HttpSecurity http) throws Exception {
        http.csrf(AbstractHttpConfigurer::disable)
                .authorizeHttpRequests(
                        auth -> auth
                                .requestMatchers(HttpMethod.OPTIONS, "/**").permitAll()
                                .requestMatchers(new AntPathRequestMatcher("/auth/login")).permitAll()
                                .requestMatchers("/actuator/health").permitAll()
                                .requestMatchers("/").permitAll()
                                .requestMatchers("/swagger-ui.html").permitAll()
                                .requestMatchers("/swagger-ui/**").permitAll()
                                .requestMatchers("/v3/api-docs").permitAll()
                                .requestMatchers("/v3/api-docs/**").permitAll()

                                .requestMatchers("/musician/{musicianId}/solo/list").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId())
                                .requestMatchers("/musician/{dni}/change-expired-password").permitAll()
                                .requestMatchers("/musician/{dni}/reset-password").permitAll()
                                .requestMatchers("/musician/dni/{dni}").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId())
                                .requestMatchers("/musician/{musicianId}/event/{eventType}/{eventId}").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers("/musician/{musicianId}/event").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers("/musician").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId())
                                .requestMatchers("/musician/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId())

                                .requestMatchers("/voice").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers("/voice/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())

                                .requestMatchers("/suggestion-box").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId())
                                .requestMatchers("/suggestion-box/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId())

                                .requestMatchers("/contact-request").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers("/contact-request/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())

                                .requestMatchers("/partiture-group").hasAnyRole(UserRoleEnum.MUSICO.getId(), UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers("/partiture-group/**").hasAnyRole(UserRoleEnum.MUSICO.getId(), UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers("/partiture").hasAnyRole(UserRoleEnum.MUSICO.getId(), UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers("/partiture/**").hasAnyRole(UserRoleEnum.MUSICO.getId(), UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())

                                .requestMatchers("/contract-group").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers("/contract-group/**").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers("/contract").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers("/contract/**").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId())

                                .requestMatchers("/cloud-document").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers("/cloud-document/**").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId())

                                .requestMatchers("/user/{username}/detail").permitAll()
                                .requestMatchers("/user/{username}/partiture-group/{partitureGroupId}").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers("/user/{username}/partiture-group").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers("/user/{username}/partiture-group/**").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId())

                                .requestMatchers("/inventory").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers("/inventory/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers("/inventory/{inventoryID}/musician").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())

                                .requestMatchers("/user/{username}/password").permitAll()
                                .requestMatchers("/user/{username}/firebase-token").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers("/user/{username}/last-date-access").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers("/user/{username}/partiture/request").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.ADMIN.getId(), UserRoleEnum.MUSICO.getId())
                                .requestMatchers("/user/partiture/request/all").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.ADMIN.getId())
                                .requestMatchers("/user/{username}/reset").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers("/user").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.ADMIN.getId(), UserRoleEnum.MUSICO.getId())
                                .requestMatchers("/user/**").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.ADMIN.getId(), UserRoleEnum.MUSICO.getId())

                                .requestMatchers("/role").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers("/role/**").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId())

                                .requestMatchers("/notification").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.ADMIN.getId())
                                .requestMatchers("/notification/**").hasAnyRole(UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.ADMIN.getId())

                                .requestMatchers("/video").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers("/video/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())

                                .requestMatchers("/video-category").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers("/video-category/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())

                                .requestMatchers("/event").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers("/event/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())

                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_CATEGORY).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers(HttpMethod.POST, "/" + ENDPOINT_REPERTOIRE_CATEGORY).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.PUT, "/" + ENDPOINT_REPERTOIRE_CATEGORY).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.DELETE, "/" + ENDPOINT_REPERTOIRE_CATEGORY).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.HEAD, "/" + ENDPOINT_REPERTOIRE_CATEGORY).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.OPTIONS, "/" + ENDPOINT_REPERTOIRE_CATEGORY).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_CATEGORY + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_CATEGORY + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_CATEGORY + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_CATEGORY + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_CATEGORY + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_CATEGORY + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())

                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_MARCH).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers(HttpMethod.POST, "/" + ENDPOINT_REPERTOIRE_MARCH).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.PUT, "/" + ENDPOINT_REPERTOIRE_MARCH).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.DELETE, "/" + ENDPOINT_REPERTOIRE_MARCH).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.HEAD, "/" + ENDPOINT_REPERTOIRE_MARCH).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.OPTIONS, "/" + ENDPOINT_REPERTOIRE_MARCH).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_MARCH + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_MARCH + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_MARCH + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_MARCH + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_MARCH + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_MARCH + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())

                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers(HttpMethod.POST, "/" + ENDPOINT_REPERTOIRE).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.PUT, "/" + ENDPOINT_REPERTOIRE).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.DELETE, "/" + ENDPOINT_REPERTOIRE).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.HEAD, "/" + ENDPOINT_REPERTOIRE).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.OPTIONS, "/" + ENDPOINT_REPERTOIRE).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())

                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_MARCH_TYPE).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers(HttpMethod.POST, "/" + ENDPOINT_REPERTOIRE_MARCH_TYPE).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.PUT, "/" + ENDPOINT_REPERTOIRE_MARCH_TYPE).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.DELETE, "/" + ENDPOINT_REPERTOIRE_MARCH_TYPE).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.HEAD, "/" + ENDPOINT_REPERTOIRE_MARCH_TYPE).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.OPTIONS, "/" + ENDPOINT_REPERTOIRE_MARCH_TYPE).hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_MARCH_TYPE + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId(), UserRoleEnum.MUSICO.getId(), UserRoleEnum.INVITADO.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_MARCH_TYPE + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_MARCH_TYPE + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_MARCH_TYPE + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_MARCH_TYPE + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())
                                .requestMatchers(HttpMethod.GET, "/" + ENDPOINT_REPERTOIRE_MARCH_TYPE + "/**").hasAnyRole(UserRoleEnum.ADMIN.getId(), UserRoleEnum.SUPER_ADMIN.getId())

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