package com.feyconsuelo.application.usecase.repertoire;

import com.feyconsuelo.application.service.repertoire.RepertoireMarchService;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.domain.usecase.repertoire.GetAllRepertoireMarchs;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetAllRepertoireMarchsImpl implements GetAllRepertoireMarchs {

    private final RepertoireMarchService repertoireMarchService;

    @Override
    public List<RepertoireMarchResponse> execute() {
        return this.repertoireMarchService.getAll();
    }
}
