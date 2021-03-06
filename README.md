# Sobre la factibilidad de localización automatizada de domicilios geográficos

Ejemplo demostrativo de la potencialidad de localización automatizada de domicilios geográficos. Para su desarrollo se implementa una técnica de inteligencia artificial denominada árbol de decisión. 

## Planteamiento del problema

En muchas ocasiones contamos con bases de datos de direcciones y queremos, más bien, tener las coordenadas geográficas. Ahora bien, si ya se tiene una gran base de datos de rasgos geográficos, será posible tener un programa que busque las direcciones y contemple el contexto geográfico, sea tolerante a la mala escritura. Además permita casos especiales como ubicar las esquinas, colonias específicas o zonas rurales.

## Solución

El método utilizado, ID3, plantea construir un árbol de decisión para cada dirección que se quiere localizar, donde en cada nodo se define la probabilidad de fracaso. Así, una vez construido el árbol de decisión, la solución es simplemente elegir el más probable de los mejores candidatos. Aún así, podría no encontrar una solución satisfactoria, debido a falta de información o bien no estar completo el catálogo de vialidades, por ejemplo, que imposibilite su ubicación. En todo caso resulta que se obtiene la mejor solución disponible. También hay que considerar que el tiempo de ejecución depende de las direcciones mismas a ubicar, así, una base de direcciones bien capturada, normalizada y completa, tarda mucho menos que una base con problemas de captura, calles falsas y campos sin información, a este fenómeno le llamo viscosidad informática y lo que resulta es en un mayor tiempo de procesamiento.

## Aplicaciones del sistema

Si se cuenta con una gran cantidad de direcciones, resulta factible utilizar una computadora con muchos núcleos disponibles y realizar el procesamiento en paralelo para georeferenciar grandes bases de datos. Una vez obtenidas las coordenadas, se pueden realizar análisis geográficos de la información y aplicar técnicas de geo estadística.

Mario Hernández Morales, 19 de abril de 2016.