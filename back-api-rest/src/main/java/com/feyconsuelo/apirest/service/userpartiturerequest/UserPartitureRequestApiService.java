package com.feyconsuelo.apirest.service.userpartiturerequest;

import com.feyconsuelo.apirest.service.userpartiturerequest.markreadunread.MarkReadUnreadRequestPartitureService;
import com.feyconsuelo.apirest.service.userpartiturerequest.query.GetRequestPartitureService;
import com.feyconsuelo.apirest.service.userpartiturerequest.request.RequestPartitureService;
import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureRequest;
import com.feyconsuelo.openapi.api.UserPartitureRequestControllerApiDelegate;
import com.feyconsuelo.openapi.model.UserRequestPartitureGroupByUserResponseDto;
import com.feyconsuelo.openapi.model.UserRequestPartitureRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserPartitureRequestApiService implements UserPartitureRequestControllerApiDelegate {

    private final RequestPartitureService requestPartitureService;
    private final GetRequestPartitureService getRequestPartitureService;
    private final MarkReadUnreadRequestPartitureService markReadUnreadRequestPartitureService;

    @Override
    public ResponseEntity<Void> requestPartiture(final String username,
                                                 final UserRequestPartitureRequestDto userRequestPartitureRequestDto) {
        return this.requestPartitureService.requestPartiture(
                UserRequestPartitureRequest.builder()
                        .username(username)
                        .description(userRequestPartitureRequestDto.getDescription())
                        .readed(userRequestPartitureRequestDto.getReaded())
                        .markReadUnreadNotificationMessage(userRequestPartitureRequestDto.getMarkReadUnreadNotificationMessage())
                        .build()
        );
    }

    @Override
    public ResponseEntity<List<UserRequestPartitureGroupByUserResponseDto>> getAllRequestPartitureGroupByUser(final Boolean all) {
        return this.getRequestPartitureService.getAllRequestPartitureGroupByUser(all);
    }

    @Override
    public ResponseEntity<Void> markReadUnreadRequestPartiture(final String username,
                                                               final UserRequestPartitureRequestDto userRequestPartitureRequestDto) {
        return this.markReadUnreadRequestPartitureService.markReadUnread(
                UserRequestPartitureRequest.builder()
                        .id(userRequestPartitureRequestDto.getId())
                        .username(username)
                        .description(userRequestPartitureRequestDto.getDescription())
                        .readed(userRequestPartitureRequestDto.getReaded())
                        .markReadUnreadNotificationMessage(userRequestPartitureRequestDto.getMarkReadUnreadNotificationMessage())
                        .build()
        );
    }

}
