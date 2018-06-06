# PD_Proyecto
Autor: Hiram Ehecatl Lujano Patrana, 313095409

Este proyecto es un juego de ajedrez para dos personas, o en otras palabras,
no tiene inteligencia artificial.

Este proyecto esta separado en tres partes, la carptea Modulos, GUI y el archivo
Main.hs, basicamente en forma de Modelo, Vista, Controlador respectivamente.

La carpeta Modulos tiene dos archivos Pieza.hs y Tablero.hs, que como su nombre
lo expresa se encargan del funcionamiento de las piezas y el tablero de ajedrez.

La carpeta GUI tiene tres archivos, GUI.html servira como contenedor para
nuestro tablero, drawer.html sera quien dibuje el tablero en GUI.html y
GUIcontroller.js que basicamente sirve para poner a GUI.html en una ventana
de escritorio. Ademas de los archivos anteriores tenemos la carpeta libraries
que contiene a p5.js una biblioteca que permite dibujar graficos en javaScript
de forma parecida a processing. Lo anterior necesita Node.js version LTS para
funcionar, el cual puede conseguirse aqui:

  https://nodejs.org/en/

Y por ultimo tenemos a Main.hs el cual tiene el rol de intermediario entre la
GUI y los modulos Pieza.hs y Tablero.hs

Se recomiendo actualizar npm con lo siguiente:

  npm update

Despues en la carpeta raiz de ejecutarse el siguiente comando:

  npm install

Para confirmar que esta funcionando debe ejecutarse o siguiente:

  npm start

Debiendo apararecer una ventana con un tablero de ajedrez sin ninguna pieza.

Si lo anterior no funciona se debera cambiar el main por el que se encuentra en
linea 100 del archivo Main.hs para poder ejecutarse la interfaz de consola.

En cualquier caso para ejecutar el programa basta con:

  runhaskell Main.hs

Pero si se requiere compilacion se debe hacer con la siguiente bandera:

  ghc -threaded Main.hs
