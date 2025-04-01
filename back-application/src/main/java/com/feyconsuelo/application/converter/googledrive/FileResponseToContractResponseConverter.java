package com.feyconsuelo.application.converter.googledrive;

import com.feyconsuelo.domain.model.contract.ContractResponse;
import com.feyconsuelo.domain.model.googledrive.FileResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class FileResponseToContractResponseConverter {

    public ContractResponse convert(final FileResponse fileResponse) {
        return ContractResponse.builder()
                .name(fileResponse.getName())
                .googleId(fileResponse.getGoogleId())
                .content(fileResponse.getContent())
                .mimeType(fileResponse.getMimeType())
                .build();
    }

}
