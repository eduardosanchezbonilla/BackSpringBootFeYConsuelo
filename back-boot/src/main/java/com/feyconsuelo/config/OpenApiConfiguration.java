package com.feyconsuelo.config;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class OpenApiConfiguration {

    @Value("${info.app.name:}")
    private String appName;

    @Value("${swagger.app.title:}")
    private String title;

    @Value("${swagger.app.description:}")
    private String description;

    @Value("${swagger.app.version:1.0}")
    private String version;

    @Value("${swagger.contact.name:Eduardo Sanchez Bonilla}")
    private String contactName;

    @Value("${swagger.contact.url:http://www.feyconsuelo.com}")
    private String contactUrl;

    @Value("${swagger.contact.email:eduardosanchezbonilla@gmail.com}")
    private String contactEmail;

    @Value("${swagger.license.name:Apache License Version 2.0}")
    private String licenseName;

    @Value("${swagger.authorize:true}")
    private Boolean authorize;


    @Bean
    public OpenAPI openApi() {
        if (StringUtils.isEmpty(this.title)) {
            this.title = this.appName + " REST Service";
        }

        if (StringUtils.isEmpty(this.description)) {
            this.description = this.appName + " REST Service";
        }

        final var openApi = new OpenAPI();
        openApi.setInfo(
                new Info()
                        .title(this.title.trim())
                        .description(this.description.trim())
                        .version(this.version)
                        .contact(
                                new Contact()
                                        .name(this.contactName)
                                        .url(this.contactUrl)
                                        .email(this.contactEmail)
                        )
                        .license(
                                new License()
                                        .name(this.licenseName)
                        )
        );
        // Configurar la seguridad Bearer
        if (Boolean.TRUE.equals(this.authorize)) {
            openApi
                    .schemaRequirement("bearerAuth", new SecurityScheme()
                            .type(SecurityScheme.Type.HTTP)
                            .scheme("bearer")
                            .bearerFormat("JWT"))
                    .addSecurityItem(new SecurityRequirement().addList("bearerAuth"));
        }


        return openApi;
    }
}
