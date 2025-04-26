package com.feyconsuelo.infrastructure.entities.performance;

public interface CrossheadProjection {

    Long getCrossheadPerformanceId();

    String getStreet();

    Integer getStreetOrder();

    String getAnnotations();

    Long getCrossheadMarchPerformanceId();

    Long getMarchId();

    String getMarchName();

    Integer getMarchOrder();

    String getMarchAnnotations();
}
