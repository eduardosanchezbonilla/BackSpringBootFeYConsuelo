package com.feyconsuelo.apirest.converter.clouddocument;

import com.feyconsuelo.domain.model.clouddocument.CloudDocumentResponse;
import com.feyconsuelo.openapi.model.CloudDocumentResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class CloudDocumentResponseToCloudDocumentResponseDtoConverter {

    public CloudDocumentResponseDto convert(final CloudDocumentResponse cloudDocumentResponse) {
        return CloudDocumentResponseDto.builder()
                .name(cloudDocumentResponse.getName())
                .googleId(cloudDocumentResponse.getGoogleId())
                .content(cloudDocumentResponse.getContent())
                .mimeType(cloudDocumentResponse.getMimeType())
                .build();
    }

}
