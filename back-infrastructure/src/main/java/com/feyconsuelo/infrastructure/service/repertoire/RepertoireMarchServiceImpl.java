package com.feyconsuelo.infrastructure.service.repertoire;

import com.feyconsuelo.application.service.repertoire.RepertoireMarchService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchRequest;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.infrastructure.converter.repertoire.RepertoireMarchEntityListToRepertoireMarchResponseListConverter;
import com.feyconsuelo.infrastructure.converter.repertoire.RepertoireMarchEntityToRepertoireMarchResponseConverter;
import com.feyconsuelo.infrastructure.converter.repertoire.RepertoireMarchRequestToRepertoireMarchEntityConverter;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchEntity;
import com.feyconsuelo.infrastructure.repository.RepertoireMarchRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class RepertoireMarchServiceImpl implements RepertoireMarchService {

    private final RepertoireMarchRepository repertoireMarchRepository;
    private final RepertoireMarchRequestToRepertoireMarchEntityConverter repertoireMarchRequestToRepertoireMarchEntityConverter;
    private final RepertoireMarchEntityListToRepertoireMarchResponseListConverter repertoireMarchEntityListToRepertoireMarchResponseListConverter;
    private final RepertoireMarchEntityToRepertoireMarchResponseConverter repertoireMarchEntityToRepertoireMarchResponseConverter;

    @Override
    public void delete(final Long repertoireMarchId) {
        this.repertoireMarchRepository.deleteById(repertoireMarchId);
    }

    @Override
    public void logicalDelete(final Long repertoireMarchId) {
        final var voice = this.repertoireMarchRepository.findRepertoireMarchActiveById(repertoireMarchId);

        if (voice.isEmpty()) {
            throw new NotFoundException("No existe la marcha que desea eliminar");
        }

        this.repertoireMarchRepository.save(this.repertoireMarchRequestToRepertoireMarchEntityConverter.deleteEntity(voice.get()));
    }

    @Override
    public List<RepertoireMarchResponse> getAll() {
        final List<RepertoireMarchEntity> repertoireMarchEntityList = this.repertoireMarchRepository.findAllActives();
        return this.repertoireMarchEntityListToRepertoireMarchResponseListConverter.convert(repertoireMarchEntityList);
    }

    @Override
    public Optional<RepertoireMarchResponse> get(final Long repertoireMarchId) {
        final var repertoireMarch = this.repertoireMarchRepository.findRepertoireMarchActiveById(repertoireMarchId);
        return repertoireMarch.map(this.repertoireMarchEntityToRepertoireMarchResponseConverter::convert);
    }

    @Override
    public void insert(final RepertoireMarchRequest repertoireMarchRequest) {
        this.repertoireMarchRepository.save(
                this.repertoireMarchRequestToRepertoireMarchEntityConverter.convert(repertoireMarchRequest)
        );
    }

    @Override
    public void update(final Long repertoireMarchId, final RepertoireMarchRequest repertoireMarchRequest) {
        final var repertoireMarch = this.repertoireMarchRepository.findRepertoireMarchActiveById(repertoireMarchId);

        if (repertoireMarch.isEmpty()) {
            throw new NotFoundException("No existe la marcha que desea modificar");
        }

        this.repertoireMarchRepository.save(
                this.repertoireMarchRequestToRepertoireMarchEntityConverter.updateEntity(repertoireMarch.get(), repertoireMarchRequest)
        );
    }

    @Override
    public List<RepertoireMarchResponse> getAllByCategoryId(final Long categoryId) {
        final List<RepertoireMarchEntity> repertoireMarchEntityList = this.repertoireMarchRepository.findAllActivesByCategoryId(categoryId);
        return this.repertoireMarchEntityListToRepertoireMarchResponseListConverter.convert(repertoireMarchEntityList);
    }

    public List<RepertoireMarchResponse> getAllByTypeId(final Long typeId) {
        final List<RepertoireMarchEntity> repertoireMarchEntityList = this.repertoireMarchRepository.findAllActivesByTypeId(typeId);
        return this.repertoireMarchEntityListToRepertoireMarchResponseListConverter.convert(repertoireMarchEntityList);
    }

}
