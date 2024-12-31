package com.feyconsuelo.apirest.service.repertoire.query;

import com.feyconsuelo.apirest.converter.repertoire.RepertoireMarchGroupByTypeResponseListToRepertoireMarchGroupByTypeResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.repertoire.RepertoireMarchResponseListToRepertoireMarchResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.repertoire.RepertoireMarchResponseToRepertoireMarchResponseDtoConverter;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchGroupByTypeRequest;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchGroupByTypeResponse;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.domain.usecase.repertoire.GetAllRepertoireMarchs;
import com.feyconsuelo.domain.usecase.repertoire.GetCategoryRepertoireMarchsGroupByType;
import com.feyconsuelo.domain.usecase.repertoire.GetRepertoireMarch;
import com.feyconsuelo.domain.usecase.repertoire.GetRepertoireMarchsGroupByType;
import com.feyconsuelo.openapi.model.RepertoireMarchGroupByTypeResponseDto;
import com.feyconsuelo.openapi.model.RepertoireMarchResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetRepertoireMarchService {

    private final GetAllRepertoireMarchs getAllRepertoireMarchs;

    private final GetRepertoireMarch getRepertoireMarch;

    private final GetCategoryRepertoireMarchsGroupByType getCategoryRepertoireMarchsGroupByType;

    private final GetRepertoireMarchsGroupByType getRepertoireMarchsGroupByType;

    private final RepertoireMarchResponseToRepertoireMarchResponseDtoConverter repertoireMarchResponseToRepertoireMarchResponseDtoConverter;

    private final RepertoireMarchResponseListToRepertoireMarchResponseDtoListConverter repertoireMarchResponseListToRepertoireMarchResponseDtoListConverter;

    private final RepertoireMarchGroupByTypeResponseListToRepertoireMarchGroupByTypeResponseDtoListConverter repertoireMarchGroupByTypeResponseListToRepertoireMarchGroupByTypeResponseDtoListConverter;

    public ResponseEntity<List<RepertoireMarchResponseDto>> getAllRepertoireMarchs() {
        final List<RepertoireMarchResponse> repertoireMarchResponseList = this.getAllRepertoireMarchs.execute();
        if (CollectionUtils.isEmpty(repertoireMarchResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.repertoireMarchResponseListToRepertoireMarchResponseDtoListConverter.convert(repertoireMarchResponseList));
    }

    public ResponseEntity<RepertoireMarchResponseDto> getRepertoireMarch(final Long repertoireMarchId) {
        final Optional<RepertoireMarchResponseDto> repertoireMarchResponse = this.getRepertoireMarch.execute(repertoireMarchId).map(this.repertoireMarchResponseToRepertoireMarchResponseDtoConverter::convert);
        return repertoireMarchResponse.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

    public ResponseEntity<List<RepertoireMarchGroupByTypeResponseDto>> getCategoryRepertoireMarchGroupByType(final RepertoireMarchGroupByTypeRequest repertoireMarchGroupByTypeRequest) {
        final List<RepertoireMarchGroupByTypeResponse> repertoire = this.getCategoryRepertoireMarchsGroupByType.execute(repertoireMarchGroupByTypeRequest);
        if (CollectionUtils.isEmpty(repertoire)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.repertoireMarchGroupByTypeResponseListToRepertoireMarchGroupByTypeResponseDtoListConverter.convert(repertoire));
    }

    public ResponseEntity<List<RepertoireMarchGroupByTypeResponseDto>> getRepertoireMarchGroupByType(final RepertoireMarchGroupByTypeRequest repertoireMarchGroupByTypeRequest) {
        final List<RepertoireMarchGroupByTypeResponse> repertoire = this.getRepertoireMarchsGroupByType.execute(repertoireMarchGroupByTypeRequest);
        if (CollectionUtils.isEmpty(repertoire)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.repertoireMarchGroupByTypeResponseListToRepertoireMarchGroupByTypeResponseDtoListConverter.convert(repertoire));
    }

}
