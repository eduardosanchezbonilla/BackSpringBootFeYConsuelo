package com.feyconsuelo.infrastructure.config.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@EnableConfigurationProperties
@ConfigurationProperties(prefix = "jwt-config")
@Data
public class JwtPropertiesConfig {
    private Integer expirationTime;
    private String secretKey;
}
