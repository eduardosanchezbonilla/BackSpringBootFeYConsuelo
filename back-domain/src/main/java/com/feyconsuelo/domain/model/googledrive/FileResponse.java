package com.feyconsuelo.domain.model.googledrive;

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
public class FileResponse {

    private String name;

    private String content;

    private String googleId;

    private String mimeType;

}
