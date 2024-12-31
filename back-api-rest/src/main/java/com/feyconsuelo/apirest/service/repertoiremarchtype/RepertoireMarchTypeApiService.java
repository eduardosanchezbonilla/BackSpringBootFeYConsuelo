package com.feyconsuelo.apirest.service.repertoiremarchtype;

import com.feyconsuelo.apirest.service.repertoiremarchtype.delete.DeleteRepertoireMarchTypeService;
import com.feyconsuelo.apirest.service.repertoiremarchtype.insert.InsertRepertoireMarchTypeService;
import com.feyconsuelo.apirest.service.repertoiremarchtype.query.GetRepertoireMarchTypeService;
import com.feyconsuelo.apirest.service.repertoiremarchtype.update.UpdateRepertoireMarchTypeService;
import com.feyconsuelo.openapi.api.RepertoireMarchTypeControllerApiDelegate;
import com.feyconsuelo.openapi.model.RepertoireMarchTypeRequestDto;
import com.feyconsuelo.openapi.model.RepertoireMarchTypeResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class RepertoireMarchTypeApiService implements RepertoireMarchTypeControllerApiDelegate {

    private final DeleteRepertoireMarchTypeService deleteRepertoireMarchTypeService;
    private final InsertRepertoireMarchTypeService insertRepertoireMarchTypeService;
    private final UpdateRepertoireMarchTypeService updateRepertoireMarchTypeService;
    private final GetRepertoireMarchTypeService getRepertoireMarchTypeService;

    @Override
    public ResponseEntity<Void> deleteRepertoireMarchType(final Long repertoireMarchTypeId) {
        return this.deleteRepertoireMarchTypeService.deleteRepertoireMarchType(repertoireMarchTypeId);
    }

    @Override
    public ResponseEntity<Void> insertRepertoireMarchType(final RepertoireMarchTypeRequestDto repertoireMarchTypeRequestDto) {
        return this.insertRepertoireMarchTypeService.insertRepertoireMarchType(repertoireMarchTypeRequestDto);
    }

    @Override
    public ResponseEntity<Void> updateRepertoireMarchType(final Long repertoireMarchTypeId,
                                                          final RepertoireMarchTypeRequestDto repertoireMarchTypeRequestDto) {
        return this.updateRepertoireMarchTypeService.updateRepertoireMarchType(repertoireMarchTypeId, repertoireMarchTypeRequestDto);
    }


    @Override
    public ResponseEntity<List<RepertoireMarchTypeResponseDto>> getAllRepertoireMarchTypes() {
        return this.getRepertoireMarchTypeService.getAllRepertoireMarchTypes();
    }

    @Override
    public ResponseEntity<RepertoireMarchTypeResponseDto> getRepertoireMarchType(final Long repertoireMarchTypeId) {
        return this.getRepertoireMarchTypeService.getRepertoireMarchType(repertoireMarchTypeId);
    }

}
