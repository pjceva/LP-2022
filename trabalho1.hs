import Data.List

type Medicamento = String
type Quantidade = Int
type EstoqueMedicamentos = [(Medicamento, Quantidade)]
type Horario = Int
type Prescricao = (Medicamento, [Horario])
type Receituario = [Prescricao]
type PlanoMedicamento = [(Horario, [Medicamento])]
type Plantao = [(Horario, [Cuidado])]
data Cuidado = Comprar Medicamento Quantidade | Medicar Medicamento
instance Show Cuidado where
  show (Comprar m q) =
    "Comprar "
      ++ Prelude.show q
      ++ " comprimido(s) do medicamento: "
      ++ m
  show (Medicar m) = "Ministrar medicamento: " ++ m

--coloca em ordem e tira repetidos
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort


--Questao 1 -> certo

--funcao para checar medicamento existe na lista
pertencer :: String -> [(String, Int)] -> Bool
pertencer nome [] = False
pertencer nome (a:as) | nome == fst(a) = True 
                      | otherwise = pertencer nome (as)

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento medicamento quantidade [] = [(medicamento, quantidade)]
comprarMedicamento medicamento quantidade (a:as)  | pertencer medicamento (a:as) == False = [(medicamento, quantidade)] ++ (a:as)
                                                  | medicamento == fst(a) = (medicamento, quantidade + snd(a)) : (as)
                                                  | otherwise = a : comprarMedicamento medicamento quantidade (as)


--Questao 2
tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento medicamento [] = Nothing
tomarMedicamento medicamento (a:as) | pertencer medicamento (a:as) == False = Nothing
                                    | medicamento == fst(a) = Just ((medicamento, snd(a) - 1) : (as))

--Questao 3
consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento medicamento [] = 0
consultarMedicamento medicamento (a:as)   | medicamento == fst(a) = snd(a)
                                          | otherwise = consultarMedicamento medicamento (as)

--Questao 4
demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos (a:as) = rmdups ((fst(a), length(snd(a))) : demandaMedicamentos (as))

--Questao 5 parte 1

--Gera uma lista com todos os medicamentos do receituario
listaMeds :: Receituario -> [String]
listaMeds [] = []
listaMeds (a:as) = fst(a) : listaMeds(as)

--checa se esta em ordem e se todos os elementos sao distintos
checarOrdemAlfabetica :: [String] -> Bool
checarOrdemAlfabetica [] = True
checarOrdemAlfabetica (a:as)  | a:as == rmdups(a:as) = True
                              | otherwise = False

checarOrdemCrescente :: Receituario -> Bool
checarOrdemCrescente [] = True
checarOrdemCrescente (a:as) | snd(a) == rmdups(snd(a)) = checarOrdemCrescente (as)
                            | otherwise = False

--checa se as codicoes sao satisfeitas
receituarioValido :: Receituario -> Bool
receituarioValido [] = True
receituarioValido x = checarOrdemAlfabetica (listaMeds x) && checarOrdemCrescente x

--Questao 5 parte 2

--Gera uma lista com todos os horarios do receituario
listaH :: PlanoMedicamento -> [Int]
listaH [] = []
listaH (a:as) = fst(a) : listaH(as)

--checa se esta em ordem e se todos os elementos sao distintos
ordemHorario :: [Int] -> Bool
ordemHorario [] = True
ordemHorario (a:as) | a:as == rmdups(a:as) = True
                    | otherwise = False

ordemMedicamento :: PlanoMedicamento -> Bool
ordemMedicamento [] = True
ordemMedicamento (a:as) | snd(a) == rmdups(snd(a)) = ordemMedicamento (as)
                        | otherwise = False

--checa se as codicoes sao satisfeitas
planoValido :: PlanoMedicamento -> Bool
planoValido [] = True
planoValido x = ordemHorario (listaH x) && ordemMedicamento x


--Questão 7

--Transforma todos os horarios em uma lista
horarios :: Receituario -> [Int]
horarios [] = []
horarios (a:as) = snd(a) ++ horarios (as)

--A partir do Receiturario gera lista com todos os horarios em ordem sem repeticao
totalHorarios :: Receituario -> [Int]
totalHorarios [] = []
totalHorarios (a:as) = rmdups (horarios(a:as))

--Verifica se um numero esta na lista
achar :: Int -> [Int] -> Bool
achar _ [] = False
achar n (x:xs)
  | x == n = True
  | otherwise = achar n xs

--recebe o primeiro elemento da lista ordenada e retorna um conjunto de medicamentos
listaMedicamentos :: Int -> Receituario -> [Medicamento]
listaMedicamentos _ [] = []
listaMedicamentos x (a:as)  | achar x (snd(a)) == False = listaMedicamentos x (as)
                            | otherwise = fst(a) : listaMedicamentos x (as)

--Percorre a lista de horarios e retorna o plano de medicamento
iteraLista :: [Int] -> Receituario -> PlanoMedicamento
iteraLista [] _ = []
iteraLista (a:as) x = (a, listaMedicamentos a x) : iteraLista as x

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario [] = []
geraPlanoReceituario x = iteraLista (totalHorarios(x)) x

--Questao 8

--Transforma todos os medicamentos em uma lista
medicamentos :: PlanoMedicamento -> [String]
medicamentos [] = []
medicamentos (a:as) = snd(a) ++ medicamentos (as)

--Lista com todos os remedios em ordem sem repeticao
totalMed :: PlanoMedicamento -> [String]
totalMed [] = []
totalMed x = rmdups(medicamentos x)

--Verifica se um medicamento esta na lista
acharMedicamento :: String -> [String] -> Bool
acharMedicamento _ [] = False
acharMedicamento x (a:as) = elem x (a:as)

--Retorna lista de horarios de certo medicamento
listaHorarios :: String -> PlanoMedicamento -> [Horario]
listaHorarios _ [] = []
listaHorarios x (a:as) | acharMedicamento x (snd(a)) == False = listaHorarios x (as)
                | otherwise = fst(a) : listaHorarios x (as)

--Percorre a lista de medicamentos e retorna um Receituario
iteraMed :: [String] -> PlanoMedicamento -> Receituario
iteraMed [] _ = []
iteraMed (a:as) x = (a, listaHorarios a x) : iteraMed as x

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano [] = [] 
geraReceituarioPlano x = iteraMed (totalMed(x)) x

--Questao 6



double :: [Int] -> [Int]
double [] = []
double (a:as) = (a*2) : double (as)
