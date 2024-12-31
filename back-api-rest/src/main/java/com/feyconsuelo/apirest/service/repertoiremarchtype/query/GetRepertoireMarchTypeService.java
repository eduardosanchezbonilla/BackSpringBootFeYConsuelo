package com.feyconsuelo.apirest.service.repertoiremarchtype.query;

import com.feyconsuelo.apirest.converter.repertoiremarchtype.RepertoireMarchTypeResponseListToRepertoireMarchTypeResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.repertoiremarchtype.RepertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter;
import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;
import com.feyconsuelo.domain.usecase.repertoiremarchtype.GetAllRepertoireMarchTypes;
import com.feyconsuelo.domain.usecase.repertoiremarchtype.GetRepertoireMarchType;
import com.feyconsuelo.openapi.model.RepertoireMarchTypeResponseDto;
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
public class GetRepertoireMarchTypeService {

    private final GetAllRepertoireMarchTypes getAllRepertoireMarchTypes;

    private final GetRepertoireMarchType getRepertoireMarchType;

    private final RepertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter repertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter;

    private final RepertoireMarchTypeResponseListToRepertoireMarchTypeResponseDtoListConverter repertoireMarchTypeResponseListToRepertoireMarchTypeResponseDtoListConverter;

    public ResponseEntity<List<RepertoireMarchTypeResponseDto>> getAllRepertoireMarchTypes() {
        final List<RepertoireMarchTypeResponse> repertoireMarchTypeResponseList = this.getAllRepertoireMarchTypes.execute();
        if (CollectionUtils.isEmpty(repertoireMarchTypeResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.repertoireMarchTypeResponseListToRepertoireMarchTypeResponseDtoListConverter.convert(repertoireMarchTypeResponseList));
    }

    public ResponseEntity<RepertoireMarchTypeResponseDto> getRepertoireMarchType(final Long repertoireMarchTypeId) {
        final Optional<RepertoireMarchTypeResponseDto> repertoireMarchTypeResponse = this.getRepertoireMarchType.execute(repertoireMarchTypeId).map(this.repertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter::convert);
        return repertoireMarchTypeResponse.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
