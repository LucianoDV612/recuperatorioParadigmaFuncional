import Text.Show.Functions()


------------punto1--------------

data Aventurero = UnAventurero {
    nombre :: String,
    salud  :: Float,
    carga :: Int,
    conservaElCoraje :: Bool,
    criterioDeSeleccionDeEncuentro :: Criterio
}deriving Show

modCriterioDeSeleccionDeEncuentro :: (Criterio -> Criterio) -> Aventurero -> Aventurero
modCriterioDeSeleccionDeEncuentro unaFuncion unAventurero = unAventurero { criterioDeSeleccionDeEncuentro = unaFuncion . criterioDeSeleccionDeEncuentro $ unAventurero}

modCarga :: (Int -> Int) -> Aventurero -> Aventurero
modCarga unaFuncion unAventurero = unAventurero { carga = unaFuncion . carga $ unAventurero}

modSalud :: (Float -> Float) -> Aventurero -> Aventurero
modSalud unaFuncion unAventurero = unAventurero { salud = unaFuncion . salud $ unAventurero}

modCoraje :: (Bool -> Bool) -> Aventurero -> Aventurero
modCoraje unaFuncion unAventurero = unAventurero { conservaElCoraje = unaFuncion . conservaElCoraje $ unAventurero}

-----Criterios de seleccion de encuentros-----

type Criterio = Aventurero -> Bool

conformista :: Criterio
conformista _ = True 

valiente :: Criterio
valiente unAventurero = conservaElCoraje unAventurero || salud unAventurero > 50

lightPacker :: Int -> Criterio
lightPacker unValor unAventurero = carga unAventurero < unValor


------------punto2--------------

existeAlguienConMasDe5Letras :: [Aventurero] -> Bool
existeAlguienConMasDe5Letras unListaDeAventureros = any (nombreMayorA5Letras) . map nombre $ unListaDeAventureros

nombreMayorA5Letras :: String -> Bool
nombreMayorA5Letras unNombre = length unNombre > 5

cargaTotal :: [Aventurero] -> Int
cargaTotal unListaDeAventureros = sum (listaDeCargasPares unListaDeAventureros)

listaDeCargasPares :: [Aventurero] -> [Int]
listaDeCargasPares unListaDeAventureros = map carga . filter (even . carga) $ unListaDeAventureros

--cargasPares :: Aventurero -> Bool
--cargasPares unAventurero =  even . carga $ unAventurero 

------------punto3--------------

type Personaje = Aventurero -> Aventurero

encuentroConPersonaje :: Aventurero -> Aventurero
encuentroConPersonaje unAventurero  = modCarga (subtract 1)  unAventurero

curandero :: Personaje
curandero unAventurero = modCarga (div 2) . modSalud (*1.2) . encuentroConPersonaje $ unAventurero

inspirador :: Personaje
inspirador unAventurero = modCoraje (const True) . modSalud (*1.1) . encuentroConPersonaje $ unAventurero

embaucador :: Personaje
embaucador unAventurero = modCoraje (const False) . modCarga (+ 10) . modSalud (*0.5) . modCriterioDeSeleccionDeEncuentro (const.lightPacker $ 10) . encuentroConPersonaje $ unAventurero

-------------punto4--------------

puedeRealizarEncuentro :: Aventurero -> Personaje -> Bool
puedeRealizarEncuentro unAventurero unPersonaje = criterioDeSeleccionDeEncuentro unAventurero (unPersonaje unAventurero)

aQueSeEnfrentaria :: Aventurero -> [Personaje] -> [Personaje]
aQueSeEnfrentaria _ [] = []
aQueSeEnfrentaria unAventurero (unPersonaje : siguientesPersonajes) 
    | criterioDeSeleccionDeEncuentro unAventurero (unPersonaje unAventurero) = unPersonaje : aQueSeEnfrentaria unAventurero siguientesPersonajes
    | otherwise = [unPersonaje]


--pepe :: Aventurero
--pepe = UnAventurero "Pepe" 50 6 False valiente

