package com.feyconsuelo.infrastructure.service.repertoirecategory;

import com.feyconsuelo.application.service.repertoirecategory.RepertoireCategoryService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryRequest;
import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryResponse;
import com.feyconsuelo.infrastructure.converter.repertoirecategory.RepertoireCategoryEntityListToRepertoireCategoryResponseListConverter;
import com.feyconsuelo.infrastructure.converter.repertoirecategory.RepertoireCategoryEntityToRepertoireCategoryResponseConverter;
import com.feyconsuelo.infrastructure.converter.repertoirecategory.RepertoireCategoryRequestToRepertoireCategoryEntityConverter;
import com.feyconsuelo.infrastructure.entities.repertoirecategory.RepertoireCategoryEntity;
import com.feyconsuelo.infrastructure.repository.RepertoireCategoryRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class RepertoireCategoryServiceImpl implements RepertoireCategoryService {

    private final RepertoireCategoryRepository repertoireCategoryRepository;
    private final RepertoireCategoryRequestToRepertoireCategoryEntityConverter repertoireCategoryRequestToRepertoireCategoryEntityConverter;
    private final RepertoireCategoryEntityListToRepertoireCategoryResponseListConverter repertoireCategoryEntityListToRepertoireCategoryResponseListConverter;
    private final RepertoireCategoryEntityToRepertoireCategoryResponseConverter repertoireCategoryEntityToRepertoireCategoryResponseConverter;

    @Override
    public void delete(final Long repertoireCategoryId) {
        this.repertoireCategoryRepository.deleteById(repertoireCategoryId);
    }

    @Override
    public void logicalDelete(final Long repertoireCategoryId) {
        final var voice = this.repertoireCategoryRepository.findRepertoireCategoryActiveById(repertoireCategoryId);

        if (voice.isEmpty()) {
            throw new NotFoundException("No existe la categoria que desea eliminar");
        }

        this.repertoireCategoryRepository.save(this.repertoireCategoryRequestToRepertoireCategoryEntityConverter.deleteEntity(voice.get()));
    }

    @Override
    public List<RepertoireCategoryResponse> getAll() {
        final List<RepertoireCategoryEntity> repertoireCategoryEntityList = this.repertoireCategoryRepository.findAllActives();
        return this.repertoireCategoryEntityListToRepertoireCategoryResponseListConverter.convert(repertoireCategoryEntityList);
    }

    @Override
    public Optional<RepertoireCategoryResponse> get(final Long repertoireCategoryId) {
        final var repertoireCategory = this.repertoireCategoryRepository.findRepertoireCategoryActiveById(repertoireCategoryId);
        return repertoireCategory.map(this.repertoireCategoryEntityToRepertoireCategoryResponseConverter::convert);
    }

    @Override
    public void insert(final RepertoireCategoryRequest repertoireCategoryRequest) {
        this.repertoireCategoryRepository.save(
                this.repertoireCategoryRequestToRepertoireCategoryEntityConverter.convert(repertoireCategoryRequest)
        );
    }

    @Override
    public void update(final Long repertoireCategoryId, final RepertoireCategoryRequest repertoireCategoryRequest) {
        final var repertoireCategory = this.repertoireCategoryRepository.findRepertoireCategoryActiveById(repertoireCategoryId);

        if (repertoireCategory.isEmpty()) {
            throw new NotFoundException("No existe la categoria que desea modificar");
        }

        this.repertoireCategoryRepository.save(
                this.repertoireCategoryRequestToRepertoireCategoryEntityConverter.updateEntity(repertoireCategory.get(), repertoireCategoryRequest)
        );
    }

}
