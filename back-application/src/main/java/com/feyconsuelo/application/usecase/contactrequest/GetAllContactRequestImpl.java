package com.feyconsuelo.application.usecase.contactrequest;

import com.feyconsuelo.application.service.contactrequest.ContactRequestService;
import com.feyconsuelo.domain.model.contactrequest.ContactResponse;
import com.feyconsuelo.domain.usecase.contactrequest.GetAllContactRequests;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Comparator;
import java.util.List;

@Component
@RequiredArgsConstructor
public class GetAllContactRequestImpl implements GetAllContactRequests {

    private final ContactRequestService contactRequestService;

    @SuppressWarnings("java:S3776")
    @Override
    public List<ContactResponse> execute(final Boolean all) {

        final List<ContactResponse> contactResponseList = this.contactRequestService.getAllContactRequest();

        if (contactResponseList.isEmpty()) {
            return List.of();
        }

        // ahora, de todos los elementos que tenemos en el array, tenemos que agrupar por user, y unir todos sus mensajes
        return contactResponseList.stream()
                .filter(contactResponse -> all || !contactResponse.getReaded())
                .sorted(Comparator.comparing(ContactResponse::getCreatedDate).reversed())
                .toList();

    }
}
