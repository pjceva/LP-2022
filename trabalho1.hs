import Data.List

type Medicamento = String
type Quantidade = Int
type EstoqueMedicamentos = [(Medicamento, Quantidade)]
type Horario = Int
type Prescricao = (Medicamento, [Horario])
type Receituario = [Prescricao]
type PlanoMedicamento = [(Horario, [Medicamento])]

--funcao para checar medicamento existe na lista
pertencer :: String -> [(String, Int)] -> Bool
pertencer nome [] = False
pertencer nome (a:as) | nome == fst(a) = True 
                      | otherwise = pertencer nome (as)


--Questao 1 -> certo
comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento medicamento quantidade [] = [(medicamento, quantidade)]
comprarMedicamento medicamento quantidade (a:as)   | pertencer medicamento (a:as) == False = [(medicamento, quantidade)] ++ (a:as)
                                                   | medicamento == fst(a) = (medicamento, quantidade + snd(a)) : (as)
                                                   | otherwise = a : comprarMedicamento medicamento quantidade (as)


--Questao 2 -> certo (ainda tem um erro mas o corretor nao pegou) 
tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento medicamento [] = Nothing
tomarMedicamento medicamento (a:as) | pertencer medicamento (a:as) == False = Nothing
                                    | medicamento == fst(a) = Just ((medicamento, snd(a) - 1) : (as))

--Questao 3 -> certo
consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento medicamento [] = 0
consultarMedicamento medicamento (a:as)   | medicamento == fst(a) = snd(a)
                                          | otherwise = consultarMedicamento medicamento (as)

--Questao 4 -> certo
demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos (a:as) = sort ((fst(a), length(snd(a))) : demandaMedicamentos (as))

--Questao 5 parte 1 -> certo
checarOrdemAlfabetica :: Receituario -> Bool
checarOrdemAlfabetica [] = True
checarOrdemAlfabetica (a:as)  | a:as == sort(a:as) = True
                              | otherwise = False

checarOrdemCrescente :: Receituario -> Bool
checarOrdemCrescente [] = True
checarOrdemCrescente (a:as) | snd(a) == sort(snd(a)) = checarOrdemCrescente (as)
                            | otherwise = False

receituarioValido :: Receituario -> Bool
receituarioValido [] = True
receituarioValido (a:as) = checarOrdemAlfabetica (a:as) && checarOrdemCrescente (a:as)

--Questao 5 parte 2 -> certo (falta verificar se repete medicamento nos dois)

ordemHorario :: PlanoMedicamento -> Bool
ordemHorario [] = True
ordemHorario (a:as)   | a:as == sort(a:as) = True
                           | otherwise = False

ordemMedicamento :: PlanoMedicamento -> Bool
ordemMedicamento [] = True
ordemMedicamento (a:as) | snd(a) == sort(snd(a)) = ordemMedicamento (as)
                        | otherwise = False

planoValido :: PlanoMedicamento -> Bool
planoValido [] = True
planoValido (a:as) = ordemHorario (a:as) && ordemMedicamento (a:as)


double :: [Int] -> [Int]
double [] = []
double (a:as) = (a*2) : double (as)
