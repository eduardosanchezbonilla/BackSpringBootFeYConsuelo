package com.feyconsuelo.infrastructure.entities.statistics;

public interface RepertoireMarchEventStatistics {
    Long getCategoryId();

    String getCategoryName();

    Long getTypeId();

    String getTypeName();

    String getTypeImage();

    Integer getTypeOrder();

    Long getMarchId();

    String getMarchName();

    String getMarchAuthor();

    Integer getQuantity();

    Double getPercentage();

    Integer getMaxQuantity();

}
