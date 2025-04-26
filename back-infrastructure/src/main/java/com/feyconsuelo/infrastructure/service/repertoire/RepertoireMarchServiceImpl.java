package com.feyconsuelo.infrastructure.service.repertoire;

import com.feyconsuelo.application.service.repertoire.RepertoireMarchService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchRequest;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchSolo;
import com.feyconsuelo.infrastructure.converter.repertoire.RepertoireMarchEntityListToRepertoireMarchResponseListConverter;
import com.feyconsuelo.infrastructure.converter.repertoire.RepertoireMarchEntityToRepertoireMarchResponseConverter;
import com.feyconsuelo.infrastructure.converter.repertoire.RepertoireMarchProjectionListToRepertoireMarchResponseListConverter;
import com.feyconsuelo.infrastructure.converter.repertoire.RepertoireMarchRequestToRepertoireMarchEntityConverter;
import com.feyconsuelo.infrastructure.converter.repertoire.RepertoireMarchSoloToRepertoireMarchSoloEntityConverter;
import com.feyconsuelo.infrastructure.converter.repertoire.RepertoireMarchSoloistToRepertoireMarchMainSoloistEntityConverter;
import com.feyconsuelo.infrastructure.converter.repertoire.RepertoireMarchSoloistToRepertoireMarchSecondarySoloistEntityConverter;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchEntity;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchProjection;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchSoloEntity;
import com.feyconsuelo.infrastructure.repository.RepertoireMarchMainSoloistRepository;
import com.feyconsuelo.infrastructure.repository.RepertoireMarchRepository;
import com.feyconsuelo.infrastructure.repository.RepertoireMarchSecondarySoloistRepository;
import com.feyconsuelo.infrastructure.repository.RepertoireMarchSoloRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

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
    private final RepertoireMarchSoloToRepertoireMarchSoloEntityConverter repertoireMarchSoloToRepertoireMarchSoloEntityConverter;
    private final RepertoireMarchMainSoloistRepository repertoireMarchMainSoloistRepository;
    private final RepertoireMarchSecondarySoloistRepository repertoireMarchSecondarySoloistRepository;
    private final RepertoireMarchSoloRepository repertoireMarchSoloRepository;
    private final RepertoireMarchSoloistToRepertoireMarchMainSoloistEntityConverter repertoireMarchSoloistToRepertoireMarchMainSoloistEntityConverter;
    private final RepertoireMarchSoloistToRepertoireMarchSecondarySoloistEntityConverter repertoireMarchSoloistToRepertoireMarchSecondarySoloistEntityConverter;
    private final RepertoireMarchProjectionListToRepertoireMarchResponseListConverter repertoireMarchProjectionListToRepertoireMarchResponseListConverter;

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
    public List<RepertoireMarchResponse> getAll(final Boolean returnSolos) {
        final List<RepertoireMarchEntity> repertoireMarchEntityList = this.repertoireMarchRepository.findAllActives();
        return this.repertoireMarchEntityListToRepertoireMarchResponseListConverter.convert(repertoireMarchEntityList, returnSolos);
    }

    @Override
    public Optional<RepertoireMarchResponse> get(final Long repertoireMarchId) {
        final var repertoireMarch = this.repertoireMarchRepository.findRepertoireMarchActiveById(repertoireMarchId);
        return repertoireMarch.map(march -> this.repertoireMarchEntityToRepertoireMarchResponseConverter.convert(march, 0, 0, Boolean.TRUE));
    }

    @Override
    public void insert(final RepertoireMarchRequest repertoireMarchRequest) {
        final RepertoireMarchEntity entity = this.repertoireMarchRepository.save(
                this.repertoireMarchRequestToRepertoireMarchEntityConverter.convert(repertoireMarchRequest)
        );

        this.updateSolosAndSoloist(entity, repertoireMarchRequest);
    }

    @Override
    public void update(final Long repertoireMarchId, final RepertoireMarchRequest repertoireMarchRequest) {
        final var repertoireMarch = this.repertoireMarchRepository.findRepertoireMarchActiveById(repertoireMarchId);

        if (repertoireMarch.isEmpty()) {
            throw new NotFoundException("No existe la marcha que desea modificar");
        }

        final RepertoireMarchEntity entity = this.repertoireMarchRepository.save(
                this.repertoireMarchRequestToRepertoireMarchEntityConverter.updateEntity(repertoireMarch.get(), repertoireMarchRequest)
        );

        this.updateSolosAndSoloist(entity, repertoireMarchRequest);
    }

    @Override
    public List<RepertoireMarchResponse> getAllByCategoryId(final Long categoryId) {
        final List<RepertoireMarchProjection> repertoireMarchProjectionList = this.repertoireMarchRepository.findAllActivesByCategoryId(categoryId);
        return this.repertoireMarchProjectionListToRepertoireMarchResponseListConverter.convert(repertoireMarchProjectionList, Boolean.TRUE);
    }

    public List<RepertoireMarchResponse> getAllByTypeId(final Long typeId) {
        final List<RepertoireMarchEntity> repertoireMarchEntityList = this.repertoireMarchRepository.findAllActivesByTypeId(typeId);
        return this.repertoireMarchEntityListToRepertoireMarchResponseListConverter.convert(repertoireMarchEntityList, Boolean.TRUE);
    }

    private void updateSolosAndSoloist(final RepertoireMarchEntity entity, final RepertoireMarchRequest repertoireMarchRequest) {
        // limpiamos todos los solos que hubiera
        if (entity.getSolos() != null) {
            entity.getSolos().clear();
            this.repertoireMarchRepository.save(entity);
        }

        // ahora si hay solos, los actualizamos
        if (!CollectionUtils.isEmpty(repertoireMarchRequest.getRepertoireMarchSolos())) {
            for (final var solo : repertoireMarchRequest.getRepertoireMarchSolos()) {
                final RepertoireMarchSoloEntity soloEntity = this.repertoireMarchSoloRepository.save(
                        this.repertoireMarchSoloToRepertoireMarchSoloEntityConverter.convert(entity, solo)
                );
                this.updateSoloist(soloEntity, solo);
            }
        }
    }

    private void updateSoloist(final RepertoireMarchSoloEntity entity, final RepertoireMarchSolo solo) {
        // limpiamos todos los solos que hubiera
        if (entity.getMainSoloists() != null) {
            entity.getMainSoloists().clear();
        }
        if (entity.getSecondarySoloists() != null) {
            entity.getSecondarySoloists().clear();
        }
        this.repertoireMarchSoloRepository.save(entity);

        // ahora si hay solistas, los actualizamos
        if (!CollectionUtils.isEmpty(solo.getMainSoloists())) {
            for (final var soloist : solo.getMainSoloists()) {
                this.repertoireMarchMainSoloistRepository.save(
                        this.repertoireMarchSoloistToRepertoireMarchMainSoloistEntityConverter.convert(entity, soloist)
                );
            }
        }
        if (!CollectionUtils.isEmpty(solo.getSecondarySoloists())) {
            for (final var soloist : solo.getSecondarySoloists()) {
                this.repertoireMarchSecondarySoloistRepository.save(
                        this.repertoireMarchSoloistToRepertoireMarchSecondarySoloistEntityConverter.convert(entity, soloist)
                );
            }
        }
    }

}
