# Trabajo Final para Paradigmas y Lenguajes de Programación
En este repositorio, se podrá encontrar todas las herramientas necesarias para la implementación de una configuración de XMonad en Ubuntu. Si bien, se puede aplicar estas configuraciones en otras distribuciones; los colores y las fuentes utilizadas se orientan a utilizar en la distribución mencionada. Un breve recorrido por la apariencia se puede observar en [este video](https://youtu.be/NPVzm3UNwec).
## Contenido del repositorio
- **aplicacion** que es un fork de [byebye](https://gitlab.com/dwt1/byebye) (aunque no puede mostrarse como tal, a causa de usar GitLab; en vez de GitHub, como en este caso), modificado para utilizarse en español y sea más compatible con un entorno que tiene instalado GNOME.
- **config** que almacena configuraciones para XMonad, XMobar, Termonad y Rofi; aplicaciones que se utilizan como parte del entorno de escritorio resultante.
- **docs** es la escritura del documento formal de este trabajo final, donde se refleja la motivación de este proyecto y se otorga información del experimento en una máquina virtual. Posee el archivo PDF (el cual debe descargarse para leerlo en un visor adecuadamente) y los archivos fuente (esto incluye las imágenes, bibliografía y archivos fuentes en LaTeX).
## Características
- El gestor de ventanas en mosaico utilizado es [XMonad](https://xmonad.org/documentation.html), aprovechando las capacidades de TreeSelect (para generar un menú similar al de inicio) y NamedScratchPads (con objetivo de controlar los periféricos de Internet y audio fácilmente, en conjunto con los procesos del sistema.
- Si bien, la barra de estado por defecto utilizada es PolyBar; se utiliza [XMobar](https://hackage.haskell.org/package/xmobar) en su lugar, gracias a que es más minimalista y atribuye a demostrar el poder de Haskell
- La terminal esperada por defecto es [Termonad](https://hackage.haskell.org/package/termonad) , la cual deberá encontrarse dentro del PATH para ser ejecutable. Cabe destacar que no se usó el paquete descargable con APT, sino que se utilizó el código desde su repositorio en GitHub y se compiló con Stack.
- Se incluye una aplicación ramificada de [byebye](https://gitlab.com/dwt1/byebye), hecha por Derek Taylor (más conocido en YouTube como DistroTube, o DT en forma abreviada); donde se tradujo al español y se cambiaron algunos de los comandos que se ejecutan en el mismo. Este programa busca poder realizar acciones sobre una sesión activa, tal como Bloquear o Cerrar Sesión; incluye hacer acciones directas sobre la computadora, como Apagar o Reiniciar. Utiliza Haskell y GTK (lo cual es beneficioso en un entorno GNOME).
- El lanzador de aplicaciones es [Rofi](https://davatorium.github.io/rofi/), donde se utiliza su navegador de archivos, el ejecutante de aplicaciones, y programas en terminal; se prefiere a dmenu porque provee la funcionalidad de por sí, evitando el requisito de conocer el lenguaje de programación C (que usa dmenu).
## Requisitos
### Pasos previos
Para poder utilizar estas configuraciones, en Ubuntu 22.04 LTS se puede ejecutar los siguientes comandos:
```sh
sudo apt install xmonad libghc-extra-dev
libghc-xmonad-contrib-dev libghc-xmonad-wallpaper-dev \
xmobar compton curl rofi scrot stterm gnome-screensaver \
g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils \
zlib1g-dev gnupg netbase git
```
#### Donde:
- xmonad es el ejecutable del gestor de ventanas.
- libghc-extra-dev provee librerías extras para GHC, necesarias en el archivo xmonad.hs modificado.
- libghc-xmonad-contrib-dev es la librería de XMonad Contrib.
- libghc-xmonad-wallpaper-dev provee la función de seleccionar aleatoriamente el fondo de pantalla desde xmonad.hs.
- xmobar es el ejecutable de la barra de estado.
- compton es el compositor de ventanas.
- curl servirá para una instrucción posterior.
- rofi es el lanzador de aplicaciones.
- scrot es una herramienta para capturar pantallas.
- stterm es una terminal minimalista que servirá para su ejecución en casos distintos a la principal (Termonad).
- gnome-screensaver sirve para bloquear la pantalla en un entorno con GDM.
- El resto de los paquetes corresponde a las dependencias dentro del siguiente comando a realizar:
```sh
curl -sSL https://get.haskellstack.org/ | sh
```
```sh
sudo apt install gobject-introspection \
libgirepository1.0-dev libgtk-3-dev \
libvte-2.91-dev libpcre2-dev
```
Donde todos los paquetes son dependencias para poder construir Termonad.
```sh
cd directorio/a/gusto
```
```sh
git clone https://github.com/cdepillabout/termonad
```
```sh
cd termonad/
```
```sh
stack build
```
```sh
stack run
```
### Configuración
Una vez hecho lo anterior, se puede clonar este repositorio y ubicar los elementos donde corresponden:
_XMonad:_ xmonad.hs debe colocarse en ~/.xmonad/
_XMobar:_ xmobarrc debe colocarse en ~/.config/xmobar/
_Termonad:_ termonad.hs debe colocarse en ~/.config/termonad/
_Rofi:_ config.rasi debe colocarse en ~/.config/rofi/
_Aplicación:_ el directorio **aplicacion** puede ubicarse donde se desee, sólo será necesario recordar dónde está; ya que se construirá la aplicación más tarde.
### Instalación
```sh
cd ~/.local/bin/
```
```sh
stack exec --package termonad --package colour -- termonad
```
```sh
cd ~/.local/bin/
```
```sh
sudo mv ∼/.cache/termonad/termonad-linux-x86_64 \
/usr/local/bin
```
```sh
cd directorio/de/aplicacion
```
```sh
stack build
```
```sh
cd .stack-work/install/unicaCarpetaAqui/hash/version/bin/
```
donde unicaCarpetaAqui posee un nombre similar al sistema operativo utilizado, hash es un conjunto de caracteres largo (se puede encontrar varios, si hay distintas construcciones sobre el mismo proyecto) y version es un número de versionado (no es el denominado para la aplicación).
```sh
sudo mv ./byebye-exe /usr/local/bin
```
```sh
xmonad --recompile
```
### Posibles pasos adicionales
Más allá de que funcione el entorno, se recomienda revisar ciertos puntos:
#### WiFi
Es muy probable que deba corregir la interfaz de WiFi en xmobarrc, ya que varía en cada computadora. Puede obtener información sobre la interfaz con iwconfig.
#### Alsamixer
Revise que alsamixer controla la fuente y el disipador de sonido correctos, así resulta más placentera la experiencia.
#### ALSA
Si ocurre que al intentar subir o bajar volumen (o silenciar tambien), no sucede lo esperado; deberá modificarse xmonad.hs para realizar el comando que funcione en su instalación de Linux.
## Finalmente...
Al iniciar sesión nuevamente, asegure de seleccionar XMonad como sesión (en vez de GNOME). Disfrute de la experiencia de un gestor de ventanas en mosaico.
