package com.feyconsuelo.infrastructure.converter.googledrive;

import com.feyconsuelo.domain.model.googledrive.FileResponse;
import com.google.api.services.drive.model.File;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class FileListToFileResponseListConverter {

    private final FileToFileResponseConverter fileToFileResponseConverter;

    public List<FileResponse> convert(final List<File> fileList) {
        if (CollectionUtils.isEmpty(fileList)) {
            return List.of();
        }
        return fileList.stream()
                .map(this.fileToFileResponseConverter::convert)
                .toList();
    }

}
