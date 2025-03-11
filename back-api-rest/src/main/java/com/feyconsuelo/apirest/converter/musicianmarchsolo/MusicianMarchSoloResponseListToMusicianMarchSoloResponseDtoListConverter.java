package com.feyconsuelo.apirest.converter.musicianmarchsolo;

import com.feyconsuelo.domain.model.musicianmarchsolo.MusicianMarchSoloResponse;
import com.feyconsuelo.openapi.model.MusicianMarchSoloResponseDto;
import com.feyconsuelo.openapi.model.MusicianSoloResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianMarchSoloResponseListToMusicianMarchSoloResponseDtoListConverter {

    private static final String TAG_MAIN_AND_SECONDARY = "(Principal y suplente)";
    private static final String TAG_MAIN = "(Principal)";
    private static final String TAG_SECONDARY = "(Suplente)";

    private String getTag(final int value, final int max) {
        if (max == 1) {
            return "Alto";
        }
        if (max < 2 || max > 5) {
            return value + "";
        }
        if (value < 1 || value > max) {
            return value + "";
        }

        final String[] tags = switch (max) {
            case 2 ->
                // Si solo hay dos niveles: 1 equivale a "Alto" y 2 a "Bajo"
                    new String[]{"Alto", "Bajo"};
            case 3 ->
                // Tres niveles: 1 "Alto", 2 "Medio" y 3 "Bajo"
                    new String[]{"Alto", "Medio", "Bajo"};
            case 4 ->
                // Por ejemplo, para 4 niveles se pueden definir: "Muy Alto", "Alto", "Bajo" y "Muy Bajo"
                    new String[]{"Muy Alto", "Alto", "Bajo", "Muy Bajo"};
            case 5 ->
                // Para 5 niveles: "Muy Alto", "Alto", "Medio", "Bajo", "Muy Bajo"
                    new String[]{"Muy Alto", "Alto", "Medio", "Bajo", "Muy Bajo"};
            default -> throw new IllegalArgumentException("Valor máximo no soportado: " + max);
        };

        // Asumiendo que el índice 0 corresponde al valor 1, 1 a 2, etc.
        return tags[value - 1];
    }

    private String getTagMainSecondary(final Integer mainSoloOrder, final Integer secondarySoloOrder) {
        if (mainSoloOrder != null && secondarySoloOrder != null) {
            return TAG_MAIN_AND_SECONDARY;
        } else if (secondarySoloOrder != null) {
            return TAG_SECONDARY;
        }
        return TAG_MAIN;
    }

    private String getSoloName(final String soloName,
                               final Integer mainSoloOrder,
                               final Integer maxMainSoloOrder,
                               final Integer secondarySoloOrder,
                               final Integer maxSecondarySoloOrder
    ) {

        if (soloName.toUpperCase().contains("SOLO") || soloName.toUpperCase().contains("REQ")) {
            return soloName + " " + this.getTagMainSecondary(mainSoloOrder, secondarySoloOrder);
        } else {
            if (mainSoloOrder != null && secondarySoloOrder != null) {
                return soloName + " (" + this.getTag(mainSoloOrder, maxMainSoloOrder) + ")" + " " + TAG_MAIN + " - (" + this.getTag(secondarySoloOrder, maxSecondarySoloOrder) + ")" + " " + TAG_SECONDARY;
            }
            if (mainSoloOrder != null) {
                return soloName + " (" + this.getTag(mainSoloOrder, maxMainSoloOrder) + ")" + " " + TAG_MAIN;
            }
            if (secondarySoloOrder != null) {
                return soloName + " (" + this.getTag(secondarySoloOrder, maxSecondarySoloOrder) + ")" + " " + TAG_SECONDARY;
            }
            return soloName + " " + TAG_MAIN;
        }
    }

    public List<MusicianMarchSoloResponseDto> convert(final List<MusicianMarchSoloResponse> musicianMarchSoloResponseList) {
        if (CollectionUtils.isEmpty(musicianMarchSoloResponseList)) {
            return List.of();
        }
        return musicianMarchSoloResponseList.stream()
                .map(musicianMarchSoloResponse ->
                        MusicianMarchSoloResponseDto.builder()
                                .marchName(musicianMarchSoloResponse.getMarchName())
                                .solos(
                                        musicianMarchSoloResponse.getSolos().stream()
                                                .map(solo ->
                                                        MusicianSoloResponseDto.builder()
                                                                .soloName(this.getSoloName(solo.getSoloName(), solo.getMainSoloistOrder(), solo.getMaxMainSoloistOrder(), solo.getSecondarySoloistOrder(), solo.getMaxSecondarySoloistOrder()).trim())
                                                                .soloOrder(solo.getSoloOrder())
                                                                .build()
                                                )
                                                .sorted(Comparator.comparing(MusicianSoloResponseDto::getSoloOrder))
                                                .toList()
                                )
                                .build()
                )
                .sorted(Comparator.comparing(MusicianMarchSoloResponseDto::getMarchName))
                .toList();
    }

}
