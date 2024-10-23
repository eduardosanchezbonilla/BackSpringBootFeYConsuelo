package com.feyconsuelo.apirest.service.user.update;

import com.feyconsuelo.application.service.utils.StringService;
import com.feyconsuelo.domain.model.user.UpdateUserDetailRequest;
import com.feyconsuelo.domain.usecase.user.UpdateUserDetail;
import com.feyconsuelo.openapi.model.UpdateUserDetailRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateUserDetailService {

    private final UpdateUserDetail updateUserDetail;

    private final StringService stringService;

    public ResponseEntity<Void> updateUserDetail(final String username,
                                                 final UpdateUserDetailRequestDto updateUserDetailRequestDto) {
        this.updateUserDetail.execute(
                UpdateUserDetailRequest.builder()
                        .username(username.toLowerCase())
                        .dni(this.stringService.toUpperCase(updateUserDetailRequestDto.getDni()))
                        .name(this.stringService.toUpperCase(updateUserDetailRequestDto.getName()))
                        .surname(this.stringService.toUpperCase(updateUserDetailRequestDto.getSurname()))
                        .direction(this.stringService.toUpperCase(updateUserDetailRequestDto.getDirection()))
                        .municipality(this.stringService.toUpperCase(updateUserDetailRequestDto.getMunicipality()))
                        .province(this.stringService.toUpperCase(updateUserDetailRequestDto.getProvince()))
                        .email(updateUserDetailRequestDto.getEmail())
                        .description(updateUserDetailRequestDto.getDescription())
                        .image(updateUserDetailRequestDto.getImage())
                        .build()
        );
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
