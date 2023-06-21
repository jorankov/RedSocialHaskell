module Redsocial where

type Usuario = (Integer, String) -- (id, nombre)

type Relacion = (Usuario, Usuario) -- usuarios que se relacionan

type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)

type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

{-La función nombresDeUsuarios llama a una función auxiliar nombresDeUsuarios' que, haciendo recursión sobre la lista de usuarios de RedSocial,
devuelve el segundo elemento de cada tupla, es decir, el nombre de cada usuario -}

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios (users, _, _) = nombresDeUsuarios' users

nombresDeUsuarios' [] = []
nombresDeUsuarios' (x : xs) = nombreDeUsuario x : nombresDeUsuarios' xs

{- La función amigosDe utiliza las funciones auxiliares amigosDe' y pertenece,
que en conjunto logran recorrer la pertenencia de los usuarios a la lista y la relacion entre ellos -}

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = amigosDe' (relaciones red) u

amigosDe' :: [Relacion] -> Usuario -> [Usuario]
amigosDe' [] _ = []
amigosDe' ((u1, u2) : rs) u
  | u == u1 = u2 : amigosDe' rs u
  | u == u2 = u1 : amigosDe' rs u
  | otherwise = amigosDe' rs u

pertenece :: (Eq t) => t -> [t] -> Bool
-- Definimos pertenece con un tipo genérico para volver a usarla más tarde con otro tipo de dato.
pertenece e [] = False
pertenece e (x : xs) = e == x || pertenece e xs

{- cantidadDeAmigos toma la lista de amigos del usuario de entrada y llama a la función auxiliar cantidadDeAmigos',
que devuelve como Int la cantidad total de amigos del usuario -}
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red user = cantidadDeAmigos' (amigosDe red user)

-- Esta función auxiliar es muy similar a la función longitud de listas (Guía 5)
cantidadDeAmigos' :: [Usuario] -> Int
cantidadDeAmigos' [] = 0
cantidadDeAmigos' (x : xs) = 1 + cantidadDeAmigos' xs

{- usuarioConMasAmigos utiliza la función cantidadDeAmigos para recorrer la cantidad total de amigos que tiene cada usuario,
en pos de encontrar al usuario con mayor cantidad, y así devolverlo en el output -}
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([u1], rs, ps) = u1
usuarioConMasAmigos ((u1 : u2 : us), rs, ps)
  | cantidadDeAmigos ((u1 : u2 : us), rs, ps) u1 >= cantidadDeAmigos ((u1 : u2 : us), rs, ps) u2 = usuarioConMasAmigos ((u1 : us), rs, ps)
  | otherwise = usuarioConMasAmigos ((u2 : us), rs, ps)

{- estaRobertoCarlos indica que, si el usuario con más amigos de toda la RedSocial tiene más de 10 amigos, entonces existe un "Roberto Carlos" en la RedSocial y es ese usuario.
Caso contrario, no existe un "Roberto Carlos" en la red, ya que todos los usuarios tienen menor o igual cantidad de amigos que el usuario con más amigos -}
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = cantidadDeAmigos red (usuarioConMasAmigos red) > 10

{- publicacionesDe toma una red y un usuario y, por medio de la funcion auxiliar, recorre la lista de publicaciones completa para filtrar las que publicó el usuario pedido -}
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red usuario = publicacionesDe' (publicaciones red) usuario

publicacionesDe' :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDe' [] _ = []
publicacionesDe' (p : ps) u
  | usuarioDePublicacion p == u = p : publicacionesDe' ps u
  | otherwise = publicacionesDe' ps u

{- publicacionesQueLeGustanA devuelve las publicaciones a las que un usuario (pasado como parámetro) les dió Me Gusta -}
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, ps) user = publicacionesQueLeGustanA' ps user

publicacionesQueLeGustanA' :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanA' [] _ = []
publicacionesQueLeGustanA' (p : ps) user
  | pertenece user (likesDePublicacion p) = p : publicacionesQueLeGustanA' ps user
  | otherwise = publicacionesQueLeGustanA' ps user

{- lesGustanLasMismasPublicaciones recorre las publicaciones que les gustan a dos usuarios pasados como parámetros,
devolviendo True si son las mismas publicaciones, o False si no lo son -}
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red user1 user2 = publicacionesQueLeGustanA red user1 == publicacionesQueLeGustanA red user2

{- tieneUnSeguidorFiel toma una red Social y un usuario como parámetros. En dicha función, y junto con las funciones auxiliares,
agarramos la lista de likes de la primer publicación del usuario pedido y analizamos primero si el primer usuario de dicha lista es seguidor fiel,
si el resultado es falso, hacemos recursión y pasamos a analizar si el segundo usuario de la lista es seguidor fiel -}
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel redSocial usuario = tieneUnSeguidorFiel' (publicacionesDe redSocial usuario)

tieneUnSeguidorFiel' :: [Publicacion] -> Bool
tieneUnSeguidorFiel' [] = False
-- Si el usuario no tiene publicaciones, entonces no tiene SF
tieneUnSeguidorFiel' (p : ps) = buscandoSeguidorFiel (likesDePublicacion p) (p : ps)

-- Buscamos primero quien le dio like a la primer publicacion del user pedido
-- Luego, tomo como segundo parametro la lista de publicaciones del usuario al q
-- queremos analizar si tiene un seguidor fiel

buscandoSeguidorFiel :: [Usuario] -> [Publicacion] -> Bool
buscandoSeguidorFiel [] _ = False
{- La [] representa los users q le dieron like a la pulicacion.
Si la primer publicacion no tiene likes, es falso ya q todas tienen q tener al menos un like
del "Seguidor fiel" -}
buscandoSeguidorFiel (u : us) (p : ps)
  | mismoLikeEnLasPublicaciones (u : us) (p : ps) = True
  {- Busca si el primer user de la lista de likes de la primer publicacion
  le dio like a las demas publicaciones. Si se cumple, es True -}

  | otherwise = buscandoSeguidorFiel (tail (u : us)) (p : ps)

{- Si no se cumple el True, quitamos al primer user de la lista y seguimos recorriendo con el siguiente, para ver si encontramos un SeguidorFiel -}

mismoLikeEnLasPublicaciones :: [Usuario] -> [Publicacion] -> Bool
mismoLikeEnLasPublicaciones (u : us) (p : ps)
  | u == usuarioDePublicacion p = buscandoSeguidorFiel us (p : ps)
  -- Si u se da autolike, no cuenta como seguidor fiel, y sigue recorriendo con los otros usuarios
  | (p : ps) == [p] && pertenece u (likesDePublicacion p) = True
  {- CB: Si solo tiene una publicacion, y le dieron like, es un SeguidorFiel -}
  | pertenece u (likesDePublicacion p) = buscandoSeguidorFiel (u : us) ps
  {- Busco si el usuario analizado esta en los likes de la publicacion. Si es True,
  hago recursion llamando a las otras publicaciones hasta dar con el CB -}
  | otherwise = False

{- Si el usuario q buscamos no le dio like a por lo menos una publicacion, dará False.
 Volviendo a buscandoSeguidorFiel, saca de la recursión a este usuario y continúa con el
 siguiente. -}

{- existeSecuenciaDeAmigos llama a una funcion auxiliar, creando una lista con el usuario 1, del que ya obtuvimos sus amigos.
La funcion auxiliar recorre la lista de amigos del usuario 1 para encontrar al usuario 2. Si este no se encuentra en dicha lista,
obtendrá la lista de amigos del primer amigo del usuario 1, y repetirá el proceso, documentando los elementos ya visitados en la lista
con ese nombre -}
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rs u1 u2
  | u1 == u2 && not (amigosDe rs u1 == []) = True -- si queremos conectar al u1 con el u1 y este tiene al menos 1 amigo, entonces existe la secuencia.
  | otherwise = existeSecuenciaDeAmigos' rs (amigosDe rs u1) [u1] u2

existeSecuenciaDeAmigos' :: RedSocial -> [Usuario] -> [Usuario] -> Usuario -> Bool
existeSecuenciaDeAmigos' rs [] _ _ = False
existeSecuenciaDeAmigos' rs (u : us) visitados u2
  | u == u2 = True
  | pertenece u visitados = existeSecuenciaDeAmigos' rs us visitados u2
  | otherwise = existeSecuenciaDeAmigos' rs (us ++ amigosDe rs u) (u : visitados) u2