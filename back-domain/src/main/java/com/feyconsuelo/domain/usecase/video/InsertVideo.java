package com.feyconsuelo.domain.usecase.video;

import com.feyconsuelo.domain.model.video.VideoRequest;

public interface InsertVideo {

    void execute(VideoRequest videoRequest);

}
