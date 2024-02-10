package com.feyconsuelo.apirest.config;

import com.feyconsuelo.apirest.dto.common.Headers;
import com.feyconsuelo.apirest.interceptor.BanSwaggerWithKong;
import com.feyconsuelo.apirest.interceptor.ClientRequestInterceptor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.web.servlet.config.annotation.*;

@Configuration
public class WebConfiguration implements WebMvcConfigurer {
    private final String allowedOrigins;
    private final String allowedMethods;
    private final String allowedHeaders;
    private final Integer maxAge;

    public WebConfiguration(@Value("${spring.config-cors.allowed-origins}") final String allowedOrigins,
                            @Value("${spring.config-cors.allowed-methods}") final String allowedMethods,
                            @Value("${spring.config-cors.allowed-headers}") final String allowedHeaders,
                            @Value("${spring.config-cors.max-age}") final Integer maxAge) {
        this.allowedOrigins = allowedOrigins;
        this.allowedMethods = allowedMethods;
        this.allowedHeaders = allowedHeaders;
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
    public void addInterceptors(final InterceptorRegistry registry) {
        final var excludePartSwagger = "/swagger*/**";
        final var excludePartActuator = "/actuator*/**";
        final var excludePartWebjars = "/webjars/**";

        registry.addInterceptor(this.clientRequestInterceptor())
                .addPathPatterns("/**")
                .excludePathPatterns(excludePartSwagger, excludePartActuator, "/");
        registry.addInterceptor(this.banSwaggerWithKong())
                .addPathPatterns(excludePartSwagger, excludePartWebjars);
    }


    @Override
    public void addCorsMappings(final CorsRegistry registry) {
        registry.addMapping("/**")
                .allowedOrigins(this.allowedOrigins)
                .allowedMethods(this.allowedMethods.split(","))
                .allowedHeaders(this.allowedHeaders.split(","))
                .maxAge(this.maxAge);
    }

    @Bean(name = "headers")
    @Scope(value = "request", proxyMode = ScopedProxyMode.TARGET_CLASS)
    public Headers clientEntity() {
        return new Headers();
    }

    //Define interceptors
    @Bean
    public ClientRequestInterceptor clientRequestInterceptor() {
        return new ClientRequestInterceptor(this.clientEntity());
    }

    @Bean
    public BanSwaggerWithKong banSwaggerWithKong() {
        return new BanSwaggerWithKong();
    }

}
