package com.feyconsuelo.application.converter.googledrive;

import com.feyconsuelo.domain.model.clouddocument.CloudDocumentResponse;
import com.feyconsuelo.domain.model.googledrive.FileResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class FileResponseListToCloudDocumentResponseListConverter {

    private final FileResponseToCloudDocumentResponseConverter fileResponseToCloudDocumentResponseConverter;

    public List<CloudDocumentResponse> convert(final List<FileResponse> fileResponseList) {
        if (CollectionUtils.isEmpty(fileResponseList)) {
            return List.of();
        }
        return fileResponseList.stream()
                .map(this.fileResponseToCloudDocumentResponseConverter::convert)
                .toList();
    }

}
