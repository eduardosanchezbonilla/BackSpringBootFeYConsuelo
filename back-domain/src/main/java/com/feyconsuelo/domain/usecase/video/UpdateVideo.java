package com.feyconsuelo.domain.usecase.video;

import com.feyconsuelo.domain.model.video.VideoRequest;

public interface UpdateVideo {

    void execute(Long videoId, VideoRequest videoRequest);

}
