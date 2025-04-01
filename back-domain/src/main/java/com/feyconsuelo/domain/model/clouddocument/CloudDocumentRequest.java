package com.feyconsuelo.domain.model.clouddocument;

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
public class CloudDocumentRequest {

    private String folderGoogleId;

    private String name;

    private String content;

    private String mimeType;

}
