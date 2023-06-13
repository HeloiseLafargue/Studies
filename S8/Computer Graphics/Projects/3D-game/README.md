# ENUNCIADO DE LA PRÁCTICA

Vamos a crear un juego de coches basado en la plantilla que os he creado. Podéis modificar o extender cosas, conforme vayáis implementando las diferentes extensiones.
Las descripciones de las extensiones son muy generales, vosotros deberéis decidir sus detalles de implementación.

### Obstáculo:

Vamos a crear obstáculos que están distribuidos aleatoriamente por la pista.
Si el coche colisiona se frena.

### Monedas

Vamos a poner monedas distribuidas por la pista. 

### Disparo:

Vamos a habilitar la posibilidad de que el coche dispare. Cada disparo cuesta una moneda y destruye los obstáculos.


### Barrera:

Vamos a crear un obstáculo que se mueve de arriba a abajo como si fuera una compuerta. 
Si el coche colisiona con la compuerta, el coche se para.
El coche puede disparar y destruir la compuerta.

### Paso de peatones:

Vamos a crear un paso de peatones, en el que cada cierto tiempo pasa una persona caminando. 
Utilizaremos algún modelo animado.
Si el coche le atropella, le daremos 100 monedas al Player.


### La mancha de aceite:

Usando una textura en el suelo vamos a implementar una mancha de aceite. Si el coche pisa la mancha, patinará durante los siguientes segundos (podéis usar random para mover el coche de izquierda a derecha).

### Tierra:

Vamos a usar una textura en el suelo que al pasar por la misma el coche reduce su velocidad, no se frena completamente. 


### La grúa:

Vamos a colocar una grúa en la carretera, tiene que ser un objeto que gire y que tenga gancho que suba y baje
Si el coche colisiona con el gancho pierde todas las monedas.

### El arco loco:

Vamos a poner un arco con una textura animada (usando ofFbo). 
La textura tendrá un número que cambiará aleatoriamente.
Cuando el coche pase por debajo le dará tantas monedas como estén en la textura.


### El pozo:

Si el coche se cae al pozo, volverá al principio.


### El bombardero:

Vamos a poner un avión que pasa volando y tira una bomba. 
Si la bomba le cae al coche encima vuelve al principio.
Si la bomba le cae cerca, pero no encima, el coche se da la vuelta debido a la onda expansiva. 

### La carretera:

La carretera deberá ser circular, incluyendo, giros o curvas.
Podéis añadir detalles como árboles, farolas, edificios, etc..
Podéis añadir una cámara cenital para ver el recorrido.

### Pausa:

Haced un State nuevo de pausa (tened en cuenta que el cronómetro se debe parar).

### Extensiones personales.

Deberéis añadir al menos 3 extensiones propias.

### Detalles generales

Se valorarán los detalles: el menú, tipografías, la iluminación, que guarde el record, efectos de sonido...