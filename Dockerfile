# Usar una imagen base con JDK 11 y Maven
FROM maven:3.8.4-openjdk-17 AS build

# Establecer un directorio de trabajo
WORKDIR /app

# Copiar archivos de tu proyecto al directorio de trabajo
COPY . /app

# Ejecutar Maven para construir el proyecto
RUN mvn clean package

# Usa una imagen base con JDK 17
FROM eclipse-temurin:17-jdk-alpine

# 1. Instalar tzdata para disponer de las zonas horarias
RUN apk add --no-cache tzdata

# 2. Fijar la zona horaria del sistema a Europe/Madrid
ENV TZ=Europe/Madrid

# Copiar el archivo JAR construido desde la etapa anterior
COPY --from=build /app/back-boot/target/back-boot-0.0.1-SNAPSHOT.jar /app/back-boot-0.0.1-SNAPSHOT.jar

# Exponer el puerto que utilizará la aplicación
EXPOSE 8080

# Establecer el punto de entrada para ejecutar la aplicación
#ENTRYPOINT ["java","-jar","/app/back-boot-0.0.1-SNAPSHOT.jar"]
ENTRYPOINT ["java", \
           "-Duser.timezone=Europe/Madrid", \
           "-jar", \
           "/app/back-boot-0.0.1-SNAPSHOT.jar"]