package com.feyconsuelo.infrastructure.converter.rehearsal;

import com.feyconsuelo.domain.model.event.EventClsClassEnum;
import com.feyconsuelo.domain.model.event.EventMusiciansResponse;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.infrastructure.converter.performance.MusiciansStringToMusicianResponseListConverter;
import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalMusiciansProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.stream.Stream;

@Slf4j
@Component
@RequiredArgsConstructor
public class RehearsalMusiciansProjectionToEventMusiciansResponseConverter {

    private final MusiciansStringToMusicianResponseListConverter musiciansStringToMusicianResponseListConverter;

    @Value("${default-images.fake-musician}")
    private String defaultFakeMusicianImage;

    private List<VoiceResponse> getVoiceList(final RehearsalMusiciansProjection performanceEntity) {
        if (CollectionUtils.isEmpty(performanceEntity.getVoiceIdList())) {
            return List.of();
        } else {
            return performanceEntity.getVoiceIdList().stream()
                    .map(
                            id -> VoiceResponse.builder()
                                    .id(Long.valueOf(id))
                                    .build()
                    )
                    .toList();
        }
    }

    public EventMusiciansResponse convert(final RehearsalMusiciansProjection rehearsalMusiciansProjection) {

        final List<MusicianResponse> musicians = this.musiciansStringToMusicianResponseListConverter.convert(rehearsalMusiciansProjection.getMusicians());
        final List<MusicianResponse> fakeMusicians = this.musiciansStringToMusicianResponseListConverter.convert(rehearsalMusiciansProjection.getFakeMusicians());

        // a todos los fake popngo la foto del hueco
        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(fakeMusicians))) {
            fakeMusicians.forEach(musician -> musician.setImage(this.defaultFakeMusicianImage));
        }

        return EventMusiciansResponse.builder()
                .event(
                        EventResponse.builder()
                                .id(rehearsalMusiciansProjection.getRehearsalId())
                                .type(EventTypeEnum.REHEARSAL)
                                .date(rehearsalMusiciansProjection.getDate())
                                .startTime(rehearsalMusiciansProjection.getStartTime())
                                .endTime(rehearsalMusiciansProjection.getEndTime())
                                .description(rehearsalMusiciansProjection.getDescription())
                                .voiceIdList(rehearsalMusiciansProjection.getVoiceIdList())
                                .voiceList(this.getVoiceList(rehearsalMusiciansProjection))
                                .location(rehearsalMusiciansProjection.getLocation())
                                .municipality(rehearsalMusiciansProjection.getMunicipality())
                                .province(rehearsalMusiciansProjection.getProvince())
                                .clsClass(EventClsClassEnum.ENSAYO_GENERAL_DAY)
                                .duration(rehearsalMusiciansProjection.getDuration())
                                .build()
                )
                .musicians(Stream.concat(musicians.stream(), fakeMusicians.stream()).toList())
                .build();
    }
}
