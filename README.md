# Descripcion

Este proyecto se usa únicamente para generar el arquetipo madiva-project-archetype.
Esta basado en el proyecto auth pero el arquetipo resultante es generico

## Packing

With maven, just do:

```shell script
  mvn clean package 
```

## Install in local

With maven, just do:

```shell script
  mvn clean install 
```

With dependencies snapshot:
-U Force download snapshot

```shell script
  mvn clean install -U
```

#### Ejecutar Spring boot desde jar

```shell script
  mvn clean package -Pjar
```      

#### Ejecutar y cambiarn profile en Spring Boot

```shell script
  java -jar __.jar -Dspring.profiles.active=pre
```      

#### Publish to artifactory with:

```shell script
    mvn clean package deploy -Dusername=YOUR_USER -Dpassword=YOUR_PASSWORD -DskipTests
```

```shell script
    mvn clean package deploy -Dusername=admin -Dpassword=1Rt3f1ct
```

## Running Web module

Run only on coches-web module

* (From coches-web module)

```shell script
  mvn clean  spring-boot:run
```

* (From parent project)

```shell script
  mvn clean install && mvn spring-boot:run -pl ServiceEnergy-web 
```

Para saltar los test los test

```shell script
  mvn clean install -DskipTests
```

# DOCKER

Lanzamos el contenedor DOCKER

```shell script
  docker-compose config
```

```shell script
  docker-compose up --build
```

# Guardar las peticiones en TConsulta

Modificar application.yml y poner la propiedad consultaService.saveConsulta a true.
Sino pasa por por Kong no se recupera el usuario.
Tambien hay que cambiar la propiedad consultaService.saveConsulta con el contexto de la aplicacion para que pueda ser
guardada correctamente.

# Configuración para Jenkins

### Estos ficheros deben ir siempre en la raiz del proyecto para que funcione correctamente.

Para que el proyecto funcione correctamente, dentro de los ficheros **ansible.yml** y **jenkinsfile_vars.yml** hay que
cambiar las siguientes variables:

<repo_name>  --> Hay que sustituirlo por el nombre del repositorio, debería quedar así

`
<repo_name> -> ServiceDictionary
`

<service_name> --> Se sustituye por el nombre que queremos que se exponga en traefik, un ejemplo sería este:

`
<service_name> -> ServiceDictionary
`

## Configuración para procesos batch

En caso de que el proyecto también se utilice para procesar servicios Batch, hay que descomentar la línea del fichero
jenkinsfile_vars.yml en el que está la siguiente variable:
`stageProject: PRO-BATCH`

Con esta línea descomentada, siempre que se haga un commit a MASTER se subirá al cluster de PRO y al de BATCH el
servicio actualizado. Para que funcione correctamente en BATCH, debe existir un perfil de spring que se llame batch (en
minúsculas)

Por defecto el cluster de Batch siempre tiene 0 instancias, por lo que si se quiere hacer algo dentro de él habría que
actualizar el número de instancias dentro del ASG y de los contenedores corriendo dentro del fichero ansible.yml

Una vez añadida toda esta configuración básica, basta con seguir esta última configuración para configurar en Jenkins la
ejecución automática

https://docs.google.com/document/d/1qxjCrYiFE4Z4sNSDtIL4cOBP7fG1qVyBlrah84HO6zo/edit#bookmark=id.q1ks9y94efui
