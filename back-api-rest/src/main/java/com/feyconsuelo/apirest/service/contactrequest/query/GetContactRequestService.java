package com.feyconsuelo.apirest.service.contactrequest.query;

import com.feyconsuelo.apirest.converter.contactrequest.ContactResponseListToContactResponseDtoListConverter;
import com.feyconsuelo.domain.model.contactrequest.ContactResponse;
import com.feyconsuelo.domain.usecase.contactrequest.GetAllContactRequests;
import com.feyconsuelo.openapi.model.ContactResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetContactRequestService {

    private final GetAllContactRequests getAllContactRequests;

    private final ContactResponseListToContactResponseDtoListConverter contactResponseListToContactResponseDtoListConverter;

    public ResponseEntity<List<ContactResponseDto>> getAllContactRequests(final Boolean all) {
        final List<ContactResponse> contactResponseList = this.getAllContactRequests.execute(all);
        if (CollectionUtils.isEmpty(contactResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.contactResponseListToContactResponseDtoListConverter.convert(contactResponseList));
    }

}
