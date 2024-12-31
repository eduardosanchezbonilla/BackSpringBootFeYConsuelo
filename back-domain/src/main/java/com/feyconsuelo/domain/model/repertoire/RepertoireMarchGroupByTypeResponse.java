package com.feyconsuelo.domain.model.repertoire;

import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class RepertoireMarchGroupByTypeResponse {

    private RepertoireMarchTypeResponse type;

    private List<RepertoireMarchResponse> marchs;

}
