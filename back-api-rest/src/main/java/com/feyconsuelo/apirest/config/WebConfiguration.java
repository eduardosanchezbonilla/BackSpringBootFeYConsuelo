package com.feyconsuelo.apirest.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.ViewControllerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class WebConfiguration implements WebMvcConfigurer {
    private final String allowedOrigins;
    private final String allowedMethods;
    private final String allowedHeaders;
    private final String exposedHeaders;
    private final Integer maxAge;

    public WebConfiguration(@Value("${spring.config-cors.allowed-origins}") final String allowedOrigins,
                            @Value("${spring.config-cors.allowed-methods}") final String allowedMethods,
                            @Value("${spring.config-cors.allowed-headers}") final String allowedHeaders,
                            @Value("${spring.config-cors.exposed-headers}") final String exposedHeaders,
                            @Value("${spring.config-cors.max-age}") final Integer maxAge
    ) {
        this.allowedOrigins = allowedOrigins;
        this.allowedMethods = allowedMethods;
        this.allowedHeaders = allowedHeaders;
        this.exposedHeaders = exposedHeaders;
        this.maxAge = maxAge;
    }

    @Override
    public void addViewControllers(final ViewControllerRegistry registry) {
        registry.addViewController("/swagger-ui").setViewName("forward:/swagger-ui/index.html");
        registry.addRedirectViewController("/", "/swagger-ui/index.html").setContextRelative(Boolean.TRUE);
    }

    @Override
    public void addResourceHandlers(final ResourceHandlerRegistry registry) {
        registry.addResourceHandler("/**")
                .addResourceLocations("classpath:/static/");
    }

    @Override
    public void addCorsMappings(final CorsRegistry registry) {
        registry.addMapping("/**")
                .allowedOrigins(this.allowedOrigins.split(","))
                .allowedMethods(this.allowedMethods.split(","))
                .allowedHeaders(this.allowedHeaders.split(","))
                .exposedHeaders(this.exposedHeaders.split(","))
                .maxAge(this.maxAge);
    }

}
