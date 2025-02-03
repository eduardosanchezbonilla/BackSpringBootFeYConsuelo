package com.feyconsuelo.infrastructure.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchRequest;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchEntity;
import com.feyconsuelo.infrastructure.repository.RepertoireCategoryRepository;
import com.feyconsuelo.infrastructure.repository.RepertoireMarchTypeRepository;
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
public class RepertoireMarchRequestToRepertoireMarchEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;
    private final RepertoireCategoryRepository repertoireCategoryRepository;
    private final RepertoireMarchTypeRepository repertoireMarchTypeRepository;

    @Value("${default-images.repertoire-march}")
    private String defaultRepertoireMarchImage;

    private String getRepertoireMarchImage(final RepertoireMarchRequest repertoireMarchRequest) {
        if (StringUtils.isEmpty(repertoireMarchRequest.getImage())) {
            return repertoireMarchRequest.getImage();
        } else {
            if (repertoireMarchRequest.getImage().equals(this.defaultRepertoireMarchImage)) {
                return null;
            } else {
                return repertoireMarchRequest.getImage();
            }
        }
    }

    public RepertoireMarchEntity convert(final RepertoireMarchRequest repertoireMarchRequest) {
        return RepertoireMarchEntity.builder()
                .categoryEntity(this.repertoireCategoryRepository.findById(repertoireMarchRequest.getCategoryId()).orElse(null))
                .typeEntity(this.repertoireMarchTypeRepository.findById(repertoireMarchRequest.getTypeId()).orElse(null))
                .name(repertoireMarchRequest.getName())
                .author(repertoireMarchRequest.getAuthor())
                .description(repertoireMarchRequest.getDescription())
                .image(this.getRepertoireMarchImage(repertoireMarchRequest))
                .youtubeId(repertoireMarchRequest.getYoutubeId())
                .repertoireMarchModifiedUser(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public RepertoireMarchEntity updateEntity(final RepertoireMarchEntity repertoireMarchEntity,
                                              final RepertoireMarchRequest repertoireMarchRequest) {
        repertoireMarchEntity.setCategoryEntity(this.repertoireCategoryRepository.findById(repertoireMarchRequest.getCategoryId()).orElse(null));
        repertoireMarchEntity.setTypeEntity(this.repertoireMarchTypeRepository.findById(repertoireMarchRequest.getTypeId()).orElse(null));
        repertoireMarchEntity.setName(repertoireMarchRequest.getName());
        repertoireMarchEntity.setAuthor(repertoireMarchRequest.getAuthor());
        repertoireMarchEntity.setDescription(repertoireMarchRequest.getDescription());
        repertoireMarchEntity.setYoutubeId(repertoireMarchRequest.getYoutubeId());
        repertoireMarchEntity.setImage(this.getRepertoireMarchImage(repertoireMarchRequest));
        repertoireMarchEntity.setRepertoireMarchModifiedUser(this.tokenInfoExtractorService.getUsername());
        return repertoireMarchEntity;
    }

    public RepertoireMarchEntity deleteEntity(final RepertoireMarchEntity repertoireMarchEntity) {
        repertoireMarchEntity.setRepertoireMarchDeleteDate(LocalDateTime.now());
        repertoireMarchEntity.setRepertoireMarchModifiedUser(this.tokenInfoExtractorService.getUsername());

        return repertoireMarchEntity;
    }
}
