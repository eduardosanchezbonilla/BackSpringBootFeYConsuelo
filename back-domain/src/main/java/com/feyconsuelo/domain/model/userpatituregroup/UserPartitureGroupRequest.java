package com.feyconsuelo.domain.model.userpatituregroup;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class UserPartitureGroupRequest {

    private String username;

    private Long partitureGroupId;

    private LocalDateTime deletedDate;

}
