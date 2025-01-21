package com.feyconsuelo.application.usecase.utils;

import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Optional;
import java.util.regex.Pattern;

@Service
public class NumberService {

    private static final Integer DEFAULT_DECIMALS_NUMBER = 2;

    public Integer nvl(final Integer value) {
        if (value == null) {
            return 0;
        } else {
            return value;
        }
    }

    public Double nvl(final Double value) {
        if (value == null) {
            return 0.0;
        } else {
            return value;
        }
    }

    public Double bigDecimalToDouble(final BigDecimal value) {
        if (value == null) {
            return null;
        } else {
            return value.doubleValue();
        }
    }

    public Double round(final Double value) {
        return this.round(value, DEFAULT_DECIMALS_NUMBER);
    }

    public Double round(final Double value, final Integer decimalsNumber) {
        return BigDecimal.valueOf(this.nvl(value)).setScale(decimalsNumber, RoundingMode.HALF_EVEN).doubleValue();
    }

    public Optional<Double> stringToDouble(final String value) {
        try {
            String resultValue = value.replace("\"", "").replace("€", "").trim();
            // si la cadena tiene comas y puntos, tendremos que ver quien esta antes
            // si primero esta la coma, entonces la quitamos, pq es separador de miles y en un double el punto sera el separador de decimales
            if (resultValue.contains(",") && resultValue.contains(".")) {
                if (resultValue.indexOf(",") < resultValue.indexOf(".")) {
                    resultValue = resultValue.replace(",", "");
                } else {
                    resultValue = resultValue.replace(".", "");
                    resultValue = resultValue.replace(",", ".");
                }
            }
            // si llegamos aqui ya solo puede tener . o ,
            // si la cadena solo tiene coma, entonces lo consideramos separador de decimales
            if (resultValue.contains(",")) {
                resultValue = resultValue.replace(",", ".");
            }
            // si la cadena solo tiene punto, entonces lo consideramos separador de decimales y no tenemos que reemplazar nada
            return Optional.of(Double.parseDouble(resultValue));
        } catch (final Exception e) {
            // antes cualquier error de conversion, devolvemos vacio
            return Optional.empty();
        }
    }

    private Boolean isMilesSeparator(final String numeroStr) {
        final var pattern = Pattern.compile("^\\d{1,3}([.,]\\d{3}){0,2}$");
        final var matcher = pattern.matcher(numeroStr);

        return matcher.matches();
    }

    public Optional<Double> stringToDoubleWithRestrictions(final String value, final Double minPriceVehicle) {
        try {
            String resultValue = value.replace("\"", "")
                    .replace("€", "")
                    .replace("�", "")
                    .replace("\u0238", "")
                    .replace("\u20AC", "")
                    .trim();
            // si la cadena tiene comas y puntos, tendremos que ver quien esta antes
            // si primero esta la coma, entonces la quitamos, pq es separador de miles y en un double el punto sera el separador de decimales
            if (resultValue.contains(",") && resultValue.contains(".")) {
                if (resultValue.indexOf(",") < resultValue.indexOf(".")) {
                    resultValue = resultValue.replace(",", "");
                } else {
                    resultValue = resultValue.replace(".", "");
                    resultValue = resultValue.replace(",", ".");
                }
            }

            // si llegamos aqui ya solo puede tener . o ,

            // llegado este punto meteremos logica relativa al precio del vehiculo, y que vamos a considerar que al menos sea de 5 cifras, es decir,
            // que no podria resultar un precio menor de 10000

            // 1.- si la cadana solo tiene , la consideraremos separador decimal, a no ser que el numero resultante sea menor a 10000 y susceptible de ser separador de miles, en cuyo caso sera separador de miles y directamente la obviaremos
            if (resultValue.contains(",")) {
                resultValue = resultValue.replace(",", ".");
                if (Double.parseDouble(resultValue) < minPriceVehicle && Boolean.TRUE.equals(this.isMilesSeparator(resultValue))) {
                    resultValue = resultValue.replace(",", "");
                }
            }

            // 2.- si la cadana solo tiene . la consideraremos separador decimal, a no ser que el numero resultante sea menor a 10000 y susceptible de ser separador de miles, en cuyo caso sera separador de miles y directamente la obviaremos
            if (resultValue.contains(".") &&
                    Double.parseDouble(resultValue) < minPriceVehicle && Boolean.TRUE.equals(this.isMilesSeparator(resultValue))) {
                resultValue = resultValue.replace(".", "");
            }
            return Optional.of(Double.parseDouble(resultValue));
        } catch (final Exception e) {
            // antes cualquier error de conversion, devolvemos vacio
            return Optional.empty();
        }
    }

    public Optional<Integer> stringToInteger(final String value) {
        try {
            return Optional.of(Integer.parseInt(value));
        } catch (final Exception e) {
            // antes cualquier error de conversion, devolvemos vacio
            return Optional.empty();
        }
    }

}
