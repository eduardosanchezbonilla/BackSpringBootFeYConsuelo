package com.feyconsuelo.config;

import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cloud.bootstrap.config.BootstrapPropertySource;
import org.springframework.core.env.AbstractEnvironment;
import org.springframework.core.env.EnumerablePropertySource;
import org.springframework.core.env.PropertySource;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

@Slf4j
@Component
public class PropertiesLogger {


    final AbstractEnvironment environment;

    public PropertiesLogger(final AbstractEnvironment environment) {
        this.environment = environment;
    }

    @PostConstruct
    public void printProperties() {
        for (final EnumerablePropertySource<?> propertySource : this.findPropertiesPropertySources()) {
            final String[] propertyNames = propertySource.getPropertyNames();
            Arrays.sort(propertyNames);
            for (final String propertyName : propertyNames) {
                try {
                    final var resolvedProperty = Objects.requireNonNull(this.environment.getProperty(propertyName));
                    final var sourceProperty = Objects.requireNonNull(propertySource.getProperty(propertyName)).toString();
                    if (resolvedProperty.equals(sourceProperty)) {
                        log.info("{}={}", propertyName, resolvedProperty);
                    } else {
                        log.info("{}={} OVERRIDDEN to {}", propertyName, sourceProperty, resolvedProperty);
                    }
                } catch (final Exception e) {
                    log.info("No se ha puede mostrar valor para: {}", propertyName);
                }
            }
        }
    }

    private List<EnumerablePropertySource<?>> findPropertiesPropertySources() {
        final List<EnumerablePropertySource<?>> propertiesPropertySources = new LinkedList<>();
        for (final PropertySource<?> propertySource : this.environment.getPropertySources()) {
            if (propertySource instanceof BootstrapPropertySource) {
                propertiesPropertySources.add((EnumerablePropertySource<?>) propertySource);
            }
        }
        return propertiesPropertySources;
    }

}