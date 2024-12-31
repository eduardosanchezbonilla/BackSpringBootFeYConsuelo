package com.feyconsuelo.apirest.service.repertoiremarchtype.delete;

import com.feyconsuelo.domain.usecase.repertoiremarchtype.DeleteRepertoireMarchType;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteRepertoireMarchTypeService {

    private final DeleteRepertoireMarchType deleteRepertoireMarchType;

    public ResponseEntity<Void> deleteRepertoireMarchType(final Long repertoireMarchTypeId) {
        this.deleteRepertoireMarchType.execute(repertoireMarchTypeId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
