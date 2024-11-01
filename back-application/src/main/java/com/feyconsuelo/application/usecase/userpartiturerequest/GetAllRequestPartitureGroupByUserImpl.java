package com.feyconsuelo.application.usecase.userpartiturerequest;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.userpartiturerequest.UserPartitureRequestService;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.model.user.UserRoleEnum;
import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureGroupByUserResponse;
import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureResponse;
import com.feyconsuelo.domain.usecase.userpartiturerequest.GetAllRequestPartitureGroupByUser;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
public class GetAllRequestPartitureGroupByUserImpl implements GetAllRequestPartitureGroupByUser {

    private final UserPartitureRequestService userPartitureRequestService;
    private final MusicianService musicianService;

    @Override
    public List<UserRequestPartitureGroupByUserResponse> execute(final Boolean all) {

        final List<UserRequestPartitureResponse> userRequestPartitureResponseList = this.userPartitureRequestService.getAllUserPartitureRequest();

        if (userRequestPartitureResponseList.isEmpty()) {
            return List.of();
        }

        // ahora, de todos los elementos que tenemos en el array, tenemos que agrupar por user, y unir todas lus desriptions
        // de las partituras solicitadas
        return userRequestPartitureResponseList.stream()
                .filter(userRequestPartitureResponse -> all || !userRequestPartitureResponse.getReaded())
                .collect(
                        Collectors.groupingBy(UserRequestPartitureResponse::getUser)
                )
                .entrySet()
                .stream()
                .map(entry -> {
                    final UserResponse user = entry.getKey();
                    if (user.getRoles().contains(UserRoleEnum.MUSICO.getId())) {
                        final Optional<MusicianResponse> musician = this.musicianService.getByDni(user.getUsername().toUpperCase());
                        if (musician.isPresent()) {
                            user.setDni(musician.get().getDni());
                            user.setName(musician.get().getName());
                            user.setSurname(musician.get().getSurname());
                            user.setDirection(musician.get().getDirection());
                            user.setMunicipality(musician.get().getMunicipality());
                            user.setProvince(musician.get().getProvince());
                            user.setEmail(musician.get().getEmail());
                        }
                    }

                    final List<UserRequestPartitureResponse> userRequestPartitureResponseListByUser = entry.getValue().stream()
                            .sorted(Comparator.comparing(UserRequestPartitureResponse::getId))
                            .toList();
                    return UserRequestPartitureGroupByUserResponse.builder()
                            .user(entry.getKey())
                            .request(userRequestPartitureResponseListByUser)
                            .build();
                })
                .sorted(Comparator.comparing(userRequestPartitureGroupByUserResponse -> userRequestPartitureGroupByUserResponse.getUser().getUsername()))
                .toList();

    }
}
