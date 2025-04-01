package com.feyconsuelo.domain.model.contractgroup;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class ContractGroupResponse {

    private Long id;

    private String name;

    private String googleId;

    private LocalDateTime deleteDate;

}
