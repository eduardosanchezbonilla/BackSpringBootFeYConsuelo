package com.feyconsuelo.infrastructure.converter.googledrive;

import com.feyconsuelo.domain.model.googledrive.FileResponse;
import com.google.api.services.drive.model.File;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;


@Slf4j
@Component
@RequiredArgsConstructor
public class FileToFileResponseConverter {

    public FileResponse convert(final File file) {
        return FileResponse.builder()
                .name(file.getName())
                .googleId(file.getId())
                .build();
    }

}
