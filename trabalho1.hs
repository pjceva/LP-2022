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


--Questão 7
horarios :: Receituario -> [Int]
horarios [] = []
horarios (a:as) = snd(a) ++ horarios (as)

rmdups :: (Ord a) => [a] -> [a] --colocar em ordem e tirar repetidos
rmdups = map head . group . sort

listaHorarios :: Receituario -> [Int] --Lista com todos os horarios em ordem sem repeticao
listaHorarios [] = []
listaHorarios (a:as) = rmdups (horarios(a:as))

achar :: Int -> [Int] -> Bool
achar _ [] = False
achar n (x:xs)
  | x == n = True
  | otherwise = achar n xs

--recebe o primeiro elemento da lista ordenada e retorna o elemento junto com um conjunto de medicamentos (chamar recursivamente por outra funcao q itera a lista)
listaMedicamentos :: Int -> Receituario -> [Medicamento]
listaMedicamentos _ [] = []
listaMedicamentos x (a:as)  | achar x (snd(a)) == False = listaMedicamentos x (as)
                            | otherwise = fst(a) : listaMedicamentos x (as)

iteraLista :: [Int] -> Receituario -> PlanoMedicamento
iteraLista [] _ = []
iteraLista (a:as) x = (a, listaMedicamentos a x) : iteraLista as x

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario [] = []
geraPlanoReceituario x = iteraLista (listaHorarios(x)) x


{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

medicamentos :: PlanoMedicamento -> [String]
medicamentos [] = []
medicamentos (a:as) = snd(a) ++ medicamentos (as)

totalMed :: PlanoMedicamento -> [String] --Lista com todos os remedios em ordem sem repeticao
totalMed [] = []
totalMed x = rmdups(medicamentos x)

acharMedicamento :: String -> [String] -> Bool
acharMedicamento _ [] = False
acharMedicamento x (a:as) = elem x (a:as)

listaH :: String -> PlanoMedicamento -> [Horario] --Retorna lista de horarios de certo medicamento
listaH _ [] = []
listaH x (a:as) | acharMedicamento x (snd(a)) == False = listaH x (as)
                | otherwise = fst(a) : listaH x (as)

iteraMed :: [String] -> PlanoMedicamento -> Receituario
iteraMed [] _ = []
iteraMed (a:as) x = (a, listaH a x) : iteraMed as x


geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano [] = [] 
geraReceituarioPlano x = iteraMed (totalMed(x)) x

double :: [Int] -> [Int]
double [] = []
double (a:as) = (a*2) : double (as)
