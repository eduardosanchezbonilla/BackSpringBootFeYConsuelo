package com.feyconsuelo.apirest.converter.clouddocument;

import com.feyconsuelo.domain.model.clouddocument.CloudDocumentResponse;
import com.feyconsuelo.openapi.model.CloudDocumentResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class CloudDocumentResponseListToCloudDocumentResponseDtoListConverter {

    private final CloudDocumentResponseToCloudDocumentResponseDtoConverter cloudDocumentResponseToCloudDocumentResponseDtoConverter;

    public List<CloudDocumentResponseDto> convert(final List<CloudDocumentResponse> cloudDocumentResponseList) {
        if (CollectionUtils.isEmpty(cloudDocumentResponseList)) {
            return List.of();
        }
        return cloudDocumentResponseList.stream()
                .map(this.cloudDocumentResponseToCloudDocumentResponseDtoConverter::convert)
                .sorted(Comparator.comparing(CloudDocumentResponseDto::getName))
                .toList();
    }

}
