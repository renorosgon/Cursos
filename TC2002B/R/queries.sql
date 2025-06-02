-- Borrar todo
/* 
Ojo esto solo es con fines de este ejercicio.
Generalmente no quieres borrar ninguna tabla
*/
DROP TABLE IF EXISTS consulta_raw;
DROP TABLE IF EXISTS cabeceras_distritales;
DROP TABLE IF EXISTS municipios;
DROP TABLE IF EXISTS circunscripciones;
DROP TABLE IF EXISTS estados;

-- CREAR LA TABLA consulta_raw
/*
Recuerda que lo recomendable es tener una tabla cruda
por eso estamos creando puros atributos varchar
*/
CREATE TABLE consulta_raw(
    CIRCUNSCRIPCION                 varchar,
    ID_ESTADO                       varchar,
    NOMBRE_ESTADO                   varchar,
    ID_DISTRITO_FEDERAL             varchar,
    CABECERA_DISTRITAL_FEDERAL      varchar,
    ID_MUNICIPIO                    varchar,
    MUNICIPIO                       varchar,
    SECCION_SEDE                    varchar,
    TIPO_MRCP                       varchar,
    ID_MRCP                         varchar,
    EXT_CONTIGUA                    varchar,
    CASILLA                         varchar,
    OPINION_SI                      varchar,
    OPINION_NO                      varchar,
    NULOS                           varchar,
    TOTAL_OPINIONES                 varchar,
    LISTA_NOMINAL                   varchar,
    ESTATUS_ACTA                    varchar,
    TRIBUNAL                        varchar,
    JUICIO                          varchar,
    OBSERVACIONES                   varchar,
    NOMBRE_ARCHIVO_UNO              varchar
);

-- CONSULTA de la tabla cruda
SELECT *
FROM consulta_raw;

-- CREAR UNA NUEVA BASE DE DATOS
-- CREAR tablas apartir de la tabla cruda
CREATE TABLE IF NOT EXISTS 
    estados(id_estado, nombre_estado) 
AS 
    SELECT
        DISTINCT CAST(id_estado AS numeric) AS id_estado, nombre_estado
    FROM
        consulta_raw
    ORDER BY
        id_estado;
-- ALTERAR UNA TABLA para AGREGAR UNA LLAVE PRIMARIA
ALTER TABLE estados ADD PRIMARY KEY (id_estado);

-- CONSULTA la tabla estados
SELECT *
FROM estados;

-- CREAR la tabla municipios
CREATE TABLE IF NOT EXISTS 
    municipios(id_estado, id_municipio, municipio) 
AS 
    SELECT
        DISTINCT 
            CAST(id_estado AS numeric) AS id_estado, 
            CAST(id_municipio AS numeric) AS id_municipio, 
            municipio AS nombre_municipio
    FROM
        consulta_raw
    ORDER BY
        id_estado, id_municipio;
        
-- AGREGAR LLAVE PRIMARIA  
ALTER TABLE 
    municipios 
ADD PRIMARY KEY 
    (id_estado, id_municipio);
    
-- AGREGAR LLAVE FOREANEA
ALTER TABLE 
    municipios  
ADD CONSTRAINT 
    fk_id_estado 
FOREIGN KEY 
    (id_estado) 
REFERENCES 
    estados (id_estado);

-- CREAR la tabla de cabeceras_distritales
CREATE TABLE IF NOT EXISTS 
    cabeceras_distritales(id_estado, id_cabecera_distrital, nombre_cabecera_distrital) 
AS 
    SELECT
        DISTINCT 
            CAST(id_estado AS numeric) AS id_estado, 
            CAST(id_distrito_federal AS numeric) AS id_cabecera_distrital, 
            cabecera_distrital_federal AS nombre_cabecera_distrital
    FROM
        consulta_raw;
        
-- AGREGAR LLAVE PRIMARIA  
ALTER TABLE 
    cabeceras_distritales 
ADD PRIMARY KEY 
    (id_estado, id_cabecera_distrital);
-- AGREGAR LLAVE FOREANEA
ALTER TABLE 
    cabeceras_distritales  
ADD CONSTRAINT 
    fk_id_estado 
FOREIGN KEY 
    (id_estado) 
REFERENCES 
    estados (id_estado);
    
-- CREAR LA TABLA circunscripciones
CREATE TABLE IF NOT EXISTS 
    circunscripciones(circunscripcion, id_estado) 
AS 
    SELECT
        DISTINCT 
            CAST(id_estado AS numeric) AS id_estado,
            CAST(circunscripcion AS numeric) AS circunscripcion
    FROM
        consulta_raw;
-- AGREGAR LLAVE PRIMARIA
ALTER TABLE 
    circunscripciones 
ADD PRIMARY KEY 
    (circunscripcion);

-- AGREGAR LLAVE FOREANEA
ALTER TABLE 
    circunscripciones 
ADD CONSTRAINT 
    fk_id_estado 
FOREIGN KEY 
    (id_estado) 
REFERENCES 
    estados (id_estado);

-- CREAR LA TABLA casillas
CREATE TABLE IF NOT EXISTS 
    casillas(
        id_estado,id_municipio, seccion_sede, id_mrcp, tipo_mrcp, casilla,
        opinion_si, opinion_no, nulos, total_opiniones, lista_nominal, 
        estatus_acta) 
AS 
    SELECT
        DISTINCT 
            CAST(id_estado AS numeric) AS id_estado,
            CAST(id_municipio AS numeric) AS id_municipio,
            CAST(seccion_sede AS numeric) AS seccion_sede,
            CAST(id_mrcp AS numeric) AS id_mcrp,
            tipo_mrcp,
            casilla,
            CAST(opinion_si AS numeric) AS opinion_si,
            CAST(opinion_no AS numeric) AS opinion_no,
            CAST(nulos AS numeric) AS nulos,
            CAST(total_opiniones AS numeric) AS total_opiniones,
            CAST(lista_nominal AS numeric) AS lista_nominal,
            estatus_acta
    FROM
        consulta_raw;
-- AGREGAR LLAVE PRIMARIA
ALTER TABLE 
    casillas
ADD PRIMARY KEY 
    (id_estado, id_municipio, seccion_sede, casilla);

-- AGREGAR LLAVE FOREANEA
ALTER TABLE 
    casillas
ADD CONSTRAINT 
    fk_id_estado 
FOREIGN KEY 
    (id_estado) 
REFERENCES 
    estados (id_estado);
    
ALTER TABLE 
    casillas
ADD CONSTRAINT 
    fk_id_municipio
FOREIGN KEY 
    (id_estado, id_municipio) 
REFERENCES 
    municipios (id_estado, id_municipio);

