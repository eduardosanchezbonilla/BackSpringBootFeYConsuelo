package com.feyconsuelo.infrastructure.converter.performance;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
@Slf4j
public class MusiciansStringToMusicianResponseListConverter {

    private final ObjectMapper mapper;

    public List<MusicianResponse> convert(final String musiciansString) {
        if (StringUtils.isEmpty(musiciansString)) {
            return List.of();
        } else {
            try {
                return this.mapper.readValue(musiciansString, new TypeReference<List<MusicianResponse>>() {
                });
            } catch (final Exception e) {
                log.error("Error converting musicians response", e);
                return List.of();
            }
        }
    }
}