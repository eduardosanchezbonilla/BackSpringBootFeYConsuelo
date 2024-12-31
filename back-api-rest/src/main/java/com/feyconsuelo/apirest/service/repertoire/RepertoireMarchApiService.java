package com.feyconsuelo.apirest.service.repertoire;

import com.feyconsuelo.apirest.service.repertoire.delete.DeleteRepertoireMarchService;
import com.feyconsuelo.apirest.service.repertoire.insert.InsertRepertoireMarchService;
import com.feyconsuelo.apirest.service.repertoire.query.GetRepertoireMarchService;
import com.feyconsuelo.apirest.service.repertoire.update.UpdateRepertoireMarchService;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchGroupByTypeRequest;
import com.feyconsuelo.openapi.api.RepertoireControllerApiDelegate;
import com.feyconsuelo.openapi.model.RepertoireMarchGroupByTypeResponseDto;
import com.feyconsuelo.openapi.model.RepertoireMarchRequestDto;
import com.feyconsuelo.openapi.model.RepertoireMarchResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class RepertoireMarchApiService implements RepertoireControllerApiDelegate {

    private final DeleteRepertoireMarchService deleteRepertoireMarchService;
    private final InsertRepertoireMarchService insertRepertoireMarchService;
    private final UpdateRepertoireMarchService updateRepertoireMarchService;
    private final GetRepertoireMarchService getRepertoireMarchService;

    @Override
    public ResponseEntity<Void> deleteRepertoireMarch(final Long repertoireMarchId) {
        return this.deleteRepertoireMarchService.deleteRepertoireMarch(repertoireMarchId);
    }

    @Override
    public ResponseEntity<Void> insertRepertoireMarch(final RepertoireMarchRequestDto repertoireMarchRequestDto) {
        return this.insertRepertoireMarchService.insertRepertoireMarch(repertoireMarchRequestDto);
    }

    @Override
    public ResponseEntity<Void> updateRepertoireMarch(final Long repertoireMarchId,
                                                      final RepertoireMarchRequestDto repertoireMarchRequestDto) {
        return this.updateRepertoireMarchService.updateRepertoireMarch(repertoireMarchId, repertoireMarchRequestDto);
    }


    @Override
    public ResponseEntity<List<RepertoireMarchResponseDto>> getAllRepertoireMarchs() {
        return this.getRepertoireMarchService.getAllRepertoireMarchs();
    }

    @Override
    public ResponseEntity<RepertoireMarchResponseDto> getRepertoireMarch(final Long repertoireMarchId) {
        return this.getRepertoireMarchService.getRepertoireMarch(repertoireMarchId);
    }

    @Override
    public ResponseEntity<List<RepertoireMarchGroupByTypeResponseDto>> getCategoryRepertoireGroupByType(final Long repertoireCategoryId,
                                                                                                        final String name) {
        return this.getRepertoireMarchService.getCategoryRepertoireMarchGroupByType(
                RepertoireMarchGroupByTypeRequest.builder()
                        .categoryId(repertoireCategoryId)
                        .name(name)
                        .current(Boolean.FALSE)
                        .build()
        );
    }

    @Override
    public ResponseEntity<List<RepertoireMarchGroupByTypeResponseDto>> getRepertoireGroupByType(final String name,
                                                                                                final Boolean current) {
        return this.getRepertoireMarchService.getRepertoireMarchGroupByType(
                RepertoireMarchGroupByTypeRequest.builder()
                        .name(name)
                        .current(current)
                        .build()
        );
    }

}
