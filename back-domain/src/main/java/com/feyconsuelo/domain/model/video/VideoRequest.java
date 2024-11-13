package com.feyconsuelo.domain.model.video;

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
public class VideoRequest {

    private String youtubeId;

    private Long videoCategoryId;

    private String name;

    private String description;

    private Integer order;

}
