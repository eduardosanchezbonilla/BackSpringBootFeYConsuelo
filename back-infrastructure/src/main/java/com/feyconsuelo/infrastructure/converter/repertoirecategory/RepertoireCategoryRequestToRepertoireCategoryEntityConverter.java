package com.feyconsuelo.infrastructure.converter.repertoirecategory;

import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryRequest;
import com.feyconsuelo.infrastructure.entities.repertoirecategory.RepertoireCategoryEntity;
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
public class RepertoireCategoryRequestToRepertoireCategoryEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    @Value("${default-images.repertoire-category}")
    private String defaultRepertoireCategoryImage;

    private String getRepertoireCategoryImage(final RepertoireCategoryRequest repertoireCategoryRequest) {
        if (StringUtils.isEmpty(repertoireCategoryRequest.getImage())) {
            return repertoireCategoryRequest.getImage();
        } else {
            if (repertoireCategoryRequest.getImage().equals(this.defaultRepertoireCategoryImage)) {
                return null;
            } else {
                return repertoireCategoryRequest.getImage();
            }
        }
    }

    public RepertoireCategoryEntity convert(final RepertoireCategoryRequest repertoireCategoryRequest) {
        return RepertoireCategoryEntity.builder()
                .name(repertoireCategoryRequest.getName())
                .order(repertoireCategoryRequest.getOrder())
                .current(repertoireCategoryRequest.getCurrent())
                .image(this.getRepertoireCategoryImage(repertoireCategoryRequest))
                .repertoireModifiedUser(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public RepertoireCategoryEntity updateEntity(final RepertoireCategoryEntity repertoireCategoryEntity,
                                                 final RepertoireCategoryRequest repertoireCategoryRequest) {
        repertoireCategoryEntity.setName(repertoireCategoryRequest.getName());
        repertoireCategoryEntity.setOrder(repertoireCategoryRequest.getOrder());
        repertoireCategoryEntity.setCurrent(repertoireCategoryRequest.getCurrent());
        repertoireCategoryEntity.setImage(this.getRepertoireCategoryImage(repertoireCategoryRequest));
        repertoireCategoryEntity.setRepertoireModifiedUser(this.tokenInfoExtractorService.getUsername());

        return repertoireCategoryEntity;
    }

    public RepertoireCategoryEntity deleteEntity(final RepertoireCategoryEntity repertoireCategoryEntity) {
        repertoireCategoryEntity.setRepertoireDeleteDate(LocalDateTime.now());
        repertoireCategoryEntity.setRepertoireModifiedUser(this.tokenInfoExtractorService.getUsername());

        return repertoireCategoryEntity;
    }
}
