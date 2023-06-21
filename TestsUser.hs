module TestsUser where

import Redsocial
import Test.HUnit

main = runTestTT tests

tests =
  test
    [ -- test suite (1)nombresDeUsuarios
      " Existen usuarios en la red:" ~: (nombresDeUsuarios red1) ~?= ["Octavio", "Jorge", "Fatima", "Inaki", "Lujan", "Joaquin", "Fabrizio", "Micaela"],
      " No existen usuarios en la red:" ~: (nombresDeUsuarios red2) ~?= [],
      -- test suite (2)amigosDe
      " Lista de amigos vacia:" ~: (amigosDe red1 usuario6) ~?= [],
      " Lista de amigos (un amigo):" ~: (amigosDe red1 usuario2) ~?= [usuario4],
      " Lista de amigos:" ~: (amigosDe red3 usuario4) ~?= [usuario1, usuario2, usuario3, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11],
      -- test suite (3)cantidadDeAmigos
      " El usuario pedido tiene amigos" ~: (cantidadDeAmigos red1 usuario4) ~?= 3,
      " El usuario pedido no tiene amigos" ~: (cantidadDeAmigos red1 usuario6) ~?= 0,
      -- test suite (4)usuarioConMasAmigos
      " Hay algun usuario que tiene al menos un amigo" ~: (usuarioConMasAmigos red1) ~?= usuario4,
      " Ningun usuario tiene amigos" ~: (usuarioConMasAmigos red5) ~?= usuario1,
      " Hay dos usuarios con la misma cantidad maxima de amigos" ~: (usuarioConMasAmigos red1) ~?= usuario4,
      -- test suite (5)estaRobertoCarlos
      " No hay ningun usuario con mas de 10 amigos" ~: (estaRobertoCarlos red1) ~?= False,
      " Hay algun usuario con exactamente 10 amigos" ~: (estaRobertoCarlos red3) ~?= False,
      " Hay algun usuario con mas de 10 amigos" ~: (estaRobertoCarlos red4) ~?= True,
      -- test suite (6)publicacionesDe
      " Hay publicacion/es del usuario pedido" ~: (publicacionesDe red1 usuario3) ~?= [publicacion3_1, publicacion3_2],
      " No hay publicaciones del usuario pedido" ~: (publicacionesDe red1 usuario6) ~?= [],
      " No existen publicaciones en la red" ~: (publicacionesDe red3 usuario8) ~?= [],
      -- test suite (7)publicacionesQueLeGustanA
      " El usuario pedido dio like" ~: (publicacionesQueLeGustanA red1 usuario1) ~?= [publicacion1_2, publicacion1_3, publicacion2_2, publicacion2_3, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2, publicacion5_1],
      " El usuario pedido no dio like" ~: (publicacionesQueLeGustanA red1 usuario6) ~?= [],
      -- test suite (8)lesGustanLasMismasPublicaciones
      " A dos usuarios pedidos les gustan distintas publicaciones" ~: (lesGustanLasMismasPublicaciones red1 usuario2 usuario3) ~?= False,
      " A dos usuarios pedidos les gustan las mismas publicaciones" ~: (lesGustanLasMismasPublicaciones red1 usuario1 usuario4) ~?= True,
      -- test suite (9)tieneUnSeguidorFiel
      " Todas las publicaciones del usuario pedido tienen like de la misma persona" ~: (tieneUnSeguidorFiel red1 usuario4) ~?= True,
      " No todas las publicaciones del usuario pedido tienen like de la misma persona" ~: (tieneUnSeguidorFiel red1 usuario2) ~?= False,
      " El usuario pedido no tiene publicaciones" ~: (tieneUnSeguidorFiel red1 usuario6) ~?= False,
      " El usuario se da autolike y es su unico seguidor fiel" ~: (tieneUnSeguidorFiel red1 usuario1) ~?= False,
      -- test suite (10)existeSecuenciaDeAmigos
      " Existe una secuencia de amigos" ~: (existeSecuenciaDeAmigos red3 usuario1 usuario2) ~?= True,
      " No existe una secuencia de amigos" ~: (existeSecuenciaDeAmigos red1 usuario1 usuario7) ~?= False,
      " El usuario1 no tiene amigos" ~: (existeSecuenciaDeAmigos red5 usuario1 usuario2) ~?= False,
      " Conectar al usuario consigo mismo cuando tiene amigos" ~: (existeSecuenciaDeAmigos red1 usuario1 usuario1) ~?= True
    ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

usuario1 = (1, "Octavio")

usuario2 = (2, "Jorge")

usuario3 = (3, "Fatima")

usuario4 = (4, "Inaki")

usuario5 = (5, "Lujan")

usuario6 = (6, "Joaquin")

usuario7 = (7, "Fabrizio")

usuario8 = (8, "Micaela")

usuario9 = (9, "Mauricio")

usuario10 = (10, "Leandro")

usuario11 = (11, "Pablo")

usuario12 = (12, "Marcos")

relacion1_4 = (usuario1, usuario4)

relacion1_5 = (usuario1, usuario5)

relacion2_4 = (usuario2, usuario4)

relacion3_4 = (usuario3, usuario4)

relacion3_5 = (usuario3, usuario5)

relacion4_5 = (usuario4, usuario5)

relacion4_6 = (usuario4, usuario6)

relacion4_7 = (usuario4, usuario7)

relacion4_8 = (usuario4, usuario8)

relacion4_9 = (usuario4, usuario9)

relacion4_10 = (usuario4, usuario10)

relacion4_11 = (usuario4, usuario11)

relacion4_12 = (usuario4, usuario12)

relacion7_8 = (usuario7, usuario8)

publicacion1_1 = (usuario1, "Hola soy Octavio", [])

publicacion1_2 = (usuario1, "Estudio Cs. de Datos", [usuario2, usuario3, usuario5, usuario1, usuario4])

publicacion1_3 = (usuario1, "Este es mi ultimo post", [usuario4, usuario1])

publicacion2_1 = (usuario2, "Hola, no soy Octavio", [usuario5])

publicacion2_2 = (usuario2, "Estoy resfriado", [usuario1, usuario3, usuario4])

publicacion2_3 = (usuario2, "Tengo seis gatos", [usuario2, usuario4, usuario1])

publicacion3_1 = (usuario3, "Tengo hambre", [usuario1, usuario5, usuario4])

publicacion3_2 = (usuario3, "Me gusta la milanesa", [usuario4, usuario2, usuario1])

publicacion4_1 = (usuario4, "Soy de San Lorenzo", [usuario1, usuario2, usuario3, usuario5, usuario4])

publicacion4_2 = (usuario4, "Soy un capo", [usuario1, usuario2, usuario3, usuario5, usuario4])

publicacion5_1 = (usuario5, "No me llamo Lujan, como cambio el nombre?", [usuario4, usuario1])

-- Red Social 1

-- Esta red la usamos para varios casos sencillos, así como en algunos casos de listas vacías.
usuarios1 = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8]

relaciones1 = [relacion1_5, relacion2_4, relacion3_4, relacion4_5, relacion3_5, relacion7_8]

publicaciones1 = [publicacion1_1, publicacion1_2, publicacion1_3, publicacion2_1, publicacion2_2, publicacion2_3, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2, publicacion5_1]

red1 = (usuarios1, relaciones1, publicaciones1)

-- Red Social 2

-- Esta red la usamos únicamente para los casos en los que la red es vacía.
usuarios2 = []

relaciones2 = []

publicaciones2 = []

red2 = (usuarios2, relaciones2, publicaciones2)

-- Red Social 3

-- Para corroborar "RobertoCarlos" sea Falso con justo 10 amigos, para ver el caso de las publicaciones vacías, y para existeCadenasDeAmigos dé True
usuarios3 = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11]

relaciones3 = [relacion1_5, relacion1_4, relacion2_4, relacion3_4, relacion4_5, relacion3_5, relacion4_6, relacion4_7, relacion4_8, relacion4_9, relacion4_10, relacion4_11]

publicaciones3 = []

red3 = (usuarios3, relaciones3, publicaciones3)

-- Red Social 4

-- Para que "RobertoCarlos" dé True
usuarios4 = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]

relaciones4 = [relacion1_5, relacion2_4, relacion3_4, relacion4_5, relacion3_5, relacion1_4, relacion4_8, relacion4_6, relacion4_7, relacion4_9, relacion4_10, relacion4_11, relacion4_12]

publicaciones4 = []

red4 = (usuarios4, relaciones4, publicaciones4)

-- Red Social 5

-- Cuando ningún usuario tiene amigos
usuarios5 = [usuario1, usuario2, usuario3]

relaciones5 = []

publicaciones5 = []

red5 = (usuarios5, relaciones5, publicaciones5)