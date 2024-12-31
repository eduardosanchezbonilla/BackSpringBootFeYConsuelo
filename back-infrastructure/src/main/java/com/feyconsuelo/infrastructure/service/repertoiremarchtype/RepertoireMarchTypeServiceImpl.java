package com.feyconsuelo.infrastructure.service.repertoiremarchtype;

import com.feyconsuelo.application.service.repertoiremarchtype.RepertoireMarchTypeService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeRequest;
import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;
import com.feyconsuelo.infrastructure.converter.repertorymarchtype.RepertoireMarchTypeEntityListToRepertoireMarchTypeResponseListConverter;
import com.feyconsuelo.infrastructure.converter.repertorymarchtype.RepertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter;
import com.feyconsuelo.infrastructure.converter.repertorymarchtype.RepertoireMarchTypeRequestToRepertoireMarchTypeEntityConverter;
import com.feyconsuelo.infrastructure.entities.repertoiremarchtype.RepertoireMarchTypeEntity;
import com.feyconsuelo.infrastructure.repository.RepertoireMarchTypeRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class RepertoireMarchTypeServiceImpl implements RepertoireMarchTypeService {

    private final RepertoireMarchTypeRepository repertoireMarchTypeRepository;
    private final RepertoireMarchTypeRequestToRepertoireMarchTypeEntityConverter repertoireMarchTypeRequestToRepertoireMarchTypeEntityConverter;
    private final RepertoireMarchTypeEntityListToRepertoireMarchTypeResponseListConverter repertoireMarchTypeEntityListToRepertoireMarchTypeResponseListConverter;
    private final RepertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter repertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter;

    @Override
    public void delete(final Long repertoireMarchTypeId) {
        this.repertoireMarchTypeRepository.deleteById(repertoireMarchTypeId);
    }

    @Override
    public void logicalDelete(final Long repertoireMarchTypeId) {
        final var voice = this.repertoireMarchTypeRepository.findRepertoireMarchTypeActiveById(repertoireMarchTypeId);

        if (voice.isEmpty()) {
            throw new NotFoundException("No existe el tipo que desea eliminar");
        }

        this.repertoireMarchTypeRepository.save(this.repertoireMarchTypeRequestToRepertoireMarchTypeEntityConverter.deleteEntity(voice.get()));
    }

    @Override
    public List<RepertoireMarchTypeResponse> getAll() {
        final List<RepertoireMarchTypeEntity> repertoireMarchTypeEntityList = this.repertoireMarchTypeRepository.findAllActives();
        return this.repertoireMarchTypeEntityListToRepertoireMarchTypeResponseListConverter.convert(repertoireMarchTypeEntityList);
    }

    @Override
    public Optional<RepertoireMarchTypeResponse> get(final Long repertoireMarchTypeId) {
        final var repertoireMarchType = this.repertoireMarchTypeRepository.findRepertoireMarchTypeActiveById(repertoireMarchTypeId);
        return repertoireMarchType.map(this.repertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter::convert);
    }

    @Override
    public void insert(final RepertoireMarchTypeRequest repertoireMarchTypeRequest) {
        this.repertoireMarchTypeRepository.save(
                this.repertoireMarchTypeRequestToRepertoireMarchTypeEntityConverter.convert(repertoireMarchTypeRequest)
        );
    }

    @Override
    public void update(final Long repertoireMarchTypeId, final RepertoireMarchTypeRequest repertoireMarchTypeRequest) {
        final var repertoireMarchType = this.repertoireMarchTypeRepository.findRepertoireMarchTypeActiveById(repertoireMarchTypeId);

        if (repertoireMarchType.isEmpty()) {
            throw new NotFoundException("No existe el tipo que desea modificar");
        }

        this.repertoireMarchTypeRepository.save(
                this.repertoireMarchTypeRequestToRepertoireMarchTypeEntityConverter.updateEntity(repertoireMarchType.get(), repertoireMarchTypeRequest)
        );
    }

}
