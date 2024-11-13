package com.feyconsuelo.domain.model.video;

import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class VideoResponse {

    private Long id;

    private String youtubeId;

    private VideoCategoryResponse videoCategory;

    private String name;

    private String description;

    private Integer order;

    private LocalDateTime deleteDate;

}
