package com.feyconsuelo.application.service.report;


import com.feyconsuelo.domain.model.musician.MusicianResponse;

import java.io.IOException;
import java.util.List;

public interface EventReportService {


    String getEventReportAssistance(
            final String title,
            final Boolean displacementBus,
            final List<MusicianResponse> musiciansAssistBus,
            final List<MusicianResponse> musiciansAssistOther,
            final List<MusicianResponse> musiciansNotAssists,
            final String base64Image, // Imagen de marca de agua opcional
            final String base64HeaderImage // Imagen de cabecera opcional
    ) throws IOException;
}
