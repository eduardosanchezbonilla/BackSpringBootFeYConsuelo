package com.feyconsuelo.application.usecase.suggestionbox;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.suggestionbox.SuggestionBoxService;
import com.feyconsuelo.application.service.user.TokenInfoExtractorService;
import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxGroupByUserResponse;
import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxResponse;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.model.user.UserRoleEnum;
import com.feyconsuelo.domain.usecase.suggestionbox.GetAllSuggestionBoxGroupByUser;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
public class GetAllSuggestionBoxGroupByUserImpl implements GetAllSuggestionBoxGroupByUser {

    private final SuggestionBoxService suggestionBoxService;
    private final MusicianService musicianService;
    private final TokenInfoExtractorService tokenInfoExtractorService;

    @SuppressWarnings("java:S3776")
    @Override
    public List<SuggestionBoxGroupByUserResponse> execute(final Boolean all) {

        final Boolean isSuperAdmin = Boolean.TRUE.equals(this.tokenInfoExtractorService.hasRole(UserRoleEnum.SUPER_ADMIN.getId()));

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
                    user.setUsername(Boolean.TRUE.equals(isSuperAdmin) ? user.getUsername() : "Anonimo");
                    user.setName(Boolean.TRUE.equals(isSuperAdmin) ? user.getName() : "Anomimo");
                    user.setSurname(Boolean.TRUE.equals(isSuperAdmin) ? user.getSurname() : "");
                    user.setDni(Boolean.TRUE.equals(isSuperAdmin) ? user.getDni() : "");
                    user.setImage(Boolean.TRUE.equals(isSuperAdmin) ? user.getImage() : "");

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
