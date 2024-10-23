package com.feyconsuelo.application.converter.googledrive;

import com.feyconsuelo.domain.model.googledrive.FileResponse;
import com.feyconsuelo.domain.model.partiture.PartitureResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class FileResponseToPartitureResponseConverter {

    public PartitureResponse convert(final FileResponse fileResponse) {
        return PartitureResponse.builder()
                .name(fileResponse.getName())
                .googleId(fileResponse.getGoogleId())
                .content(fileResponse.getContent())
                .build();
    }

}
