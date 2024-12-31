package com.feyconsuelo.infrastructure.converter.repertorymarchtype;

import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeRequest;
import com.feyconsuelo.infrastructure.entities.repertoiremarchtype.RepertoireMarchTypeEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchTypeRequestToRepertoireMarchTypeEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    @Value("${default-images.repertoire-march-type}")
    private String defaultRepertoireMarchTypeImage;

    private String getRepertoireMarchTypeImage(final RepertoireMarchTypeRequest repertoireMarchTypeRequest) {
        if (StringUtils.isEmpty(repertoireMarchTypeRequest.getImage())) {
            return repertoireMarchTypeRequest.getImage();
        } else {
            if (repertoireMarchTypeRequest.getImage().equals(this.defaultRepertoireMarchTypeImage)) {
                return null;
            } else {
                return repertoireMarchTypeRequest.getImage();
            }
        }
    }

    public RepertoireMarchTypeEntity convert(final RepertoireMarchTypeRequest repertoireMarchTypeRequest) {
        return RepertoireMarchTypeEntity.builder()
                .name(repertoireMarchTypeRequest.getName())
                .order(repertoireMarchTypeRequest.getOrder())
                .image(this.getRepertoireMarchTypeImage(repertoireMarchTypeRequest))
                .repertoireMarchTypeModifiedUser(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public RepertoireMarchTypeEntity updateEntity(final RepertoireMarchTypeEntity repertoireMarchTypeEntity,
                                                  final RepertoireMarchTypeRequest repertoireMarchTypeRequest) {
        repertoireMarchTypeEntity.setName(repertoireMarchTypeRequest.getName());
        repertoireMarchTypeEntity.setOrder(repertoireMarchTypeRequest.getOrder());
        repertoireMarchTypeEntity.setImage(this.getRepertoireMarchTypeImage(repertoireMarchTypeRequest));
        repertoireMarchTypeEntity.setRepertoireMarchTypeModifiedUser(this.tokenInfoExtractorService.getUsername());

        return repertoireMarchTypeEntity;
    }

    public RepertoireMarchTypeEntity deleteEntity(final RepertoireMarchTypeEntity repertoireMarchTypeEntity) {
        repertoireMarchTypeEntity.setRepertoireMarchTypeDeleteDate(LocalDateTime.now());
        repertoireMarchTypeEntity.setRepertoireMarchTypeModifiedUser(this.tokenInfoExtractorService.getUsername());

        return repertoireMarchTypeEntity;
    }
}
