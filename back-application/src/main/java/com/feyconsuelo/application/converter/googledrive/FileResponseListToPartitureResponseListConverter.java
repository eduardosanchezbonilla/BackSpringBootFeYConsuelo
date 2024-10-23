package com.feyconsuelo.application.converter.googledrive;

import com.feyconsuelo.domain.model.googledrive.FileResponse;
import com.feyconsuelo.domain.model.partiture.PartitureResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class FileResponseListToPartitureResponseListConverter {

    private final FileResponseToPartitureResponseConverter fileResponseToPartitureResponseConverter;

    public List<PartitureResponse> convert(final List<FileResponse> fileResponseList) {
        if (CollectionUtils.isEmpty(fileResponseList)) {
            return List.of();
        }
        return fileResponseList.stream()
                .map(this.fileResponseToPartitureResponseConverter::convert)
                .toList();
    }

}
