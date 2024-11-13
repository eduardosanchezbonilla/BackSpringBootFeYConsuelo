package com.feyconsuelo.domain.model.video;

import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;
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
public class VideoGroupByCategoryResponse {

    private VideoCategoryResponse category;

    private List<VideoResponse> videos;
}
