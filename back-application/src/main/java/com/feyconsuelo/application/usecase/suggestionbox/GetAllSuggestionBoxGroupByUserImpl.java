package com.feyconsuelo.application.usecase.suggestionbox;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.suggestionbox.SuggestionBoxService;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxGroupByUserResponse;
import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxResponse;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.model.user.UserRoleEnum;
import com.feyconsuelo.domain.usecase.suggestionbox.GetAllSuggestionBoxGroupByUser;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
public class GetAllSuggestionBoxGroupByUserImpl implements GetAllSuggestionBoxGroupByUser {

    private final SuggestionBoxService suggestionBoxService;
    private final MusicianService musicianService;

    @Override
    public List<SuggestionBoxGroupByUserResponse> execute(final Boolean all) {

        final List<SuggestionBoxResponse> suggestionBoxResponseList = this.suggestionBoxService.getAllSuggestionBox();

        if (suggestionBoxResponseList.isEmpty()) {
            return List.of();
        }

        // ahora, de todos los elementos que tenemos en el array, tenemos que agrupar por user, y unir todos sus mensajes
        return suggestionBoxResponseList.stream()
                .filter(suggestionBoxResponse -> all || !suggestionBoxResponse.getReaded())
                .collect(
                        Collectors.groupingBy(SuggestionBoxResponse::getUser)
                )
                .entrySet()
                .stream()
                .map(entry -> {
                    final UserResponse user = entry.getKey();
                    if (user.getRoles().contains(UserRoleEnum.MUSICO.getId())) {
                        final Optional<MusicianResponse> musician = this.musicianService.getByDni(user.getUsername().toUpperCase(), Boolean.TRUE);
                        if (musician.isPresent()) {
                            user.setName(musician.get().getName());
                            user.setSurname(musician.get().getSurname());
                            user.setDni(musician.get().getDni());
                            user.setMunicipality(musician.get().getMunicipality());
                            user.setDirection(musician.get().getDirection());
                            user.setEmail(musician.get().getEmail());
                            user.setProvince(musician.get().getProvince());
                        }
                    }

                    final List<SuggestionBoxResponse> suggestionBoxResponseListByUser = entry.getValue().stream()
                            .sorted(Comparator.comparing(SuggestionBoxResponse::getId))
                            .toList();
                    return SuggestionBoxGroupByUserResponse.builder()
                            .user(entry.getKey())
                            .suggestions(suggestionBoxResponseListByUser)
                            .build();
                })
                .sorted(Comparator.comparing(suggestionBoxGroupByUserResponse -> suggestionBoxGroupByUserResponse.getUser().getUsername()))
                .toList();

    }
}
