package com.feyconsuelo.apirest.converter.contactrequest;

import com.feyconsuelo.domain.model.contactrequest.ContactResponse;
import com.feyconsuelo.openapi.model.ContactResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class ContactResponseListToContactResponseDtoListConverter {

    private final ContactResponseToContactResponseDtoConverter contactResponseToContactResponseDtoConverter;

    public List<ContactResponseDto> convert(final List<ContactResponse> contactResponseList) {
        if (CollectionUtils.isEmpty(contactResponseList)) {
            return List.of();
        }
        return contactResponseList.stream()
                .map(this.contactResponseToContactResponseDtoConverter::convert)
                .toList();
    }

}
