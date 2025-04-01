package com.feyconsuelo.domain.model.contract;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class ContractRequest {

    private String contractGroupGoogleId;

    private String name;

    private String content;

    private String mimeType;

}
