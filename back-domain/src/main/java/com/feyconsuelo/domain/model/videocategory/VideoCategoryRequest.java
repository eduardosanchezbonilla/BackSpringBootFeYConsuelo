package com.feyconsuelo.domain.model.videocategory;

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
public class VideoCategoryRequest {

    private String name;

    private Boolean isPublic;

    private Integer order;

    private String image;

}