package com.feyconsuelo.domain.model.userpatituregroup;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class UserPartitureGroupResponse {

    private String username;

    private Long partitureGroupId;

    private String partitureGroupName;

    private Boolean assigned;

    private LocalDateTime deleteDate;

}
