package com.feyconsuelo.application.usecase.performance;

import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.application.service.user.TokenInfoExtractorService;
import com.feyconsuelo.domain.model.event.EventCrosshead;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.user.UserRoleEnum;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetPerformanceCrossheadImpl {

    private final PerformanceService performanceService;

    private final TokenInfoExtractorService tokenInfoExtractorService;

    public Optional<EventCrosshead> execute(final Long eventId) {

        final Optional<EventResponse> event = this.performanceService.getById(eventId, true, false);

        if (event.isPresent() &&
                (
                        (
                                Boolean.FALSE.equals(this.tokenInfoExtractorService.hasRole(UserRoleEnum.SUPER_ADMIN.getId())) &&
                                        Boolean.TRUE.equals(event.get().getCrossheadPublic())
                        )
                                ||
                                Boolean.TRUE.equals(this.tokenInfoExtractorService.hasRole(UserRoleEnum.SUPER_ADMIN.getId()))
                )
        ) {
            return this.performanceService.getCrosshead(eventId);
        }

        return Optional.empty();

    }
}
