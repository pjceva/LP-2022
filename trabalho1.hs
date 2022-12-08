{-
   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

--fazer funcao para checar existencia na lista
--caso nao exista -> ++ (medicamento, quantidade)
--caso exista -> substitiu (medicamento, quantidade)

type Medicamento = String
type Quantidade = Int
type EstoqueMedicamentos = [(Medicamento, Quantidade)]

--funcao para checar medicamento existe na lista
pertencer :: String -> [(String, Int)] -> Bool
pertencer nome [] = False
pertencer nome (a:as) | nome == fst(a) = True 
                      | otherwise = pertencer nome (as)

-- comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
-- comprarMedicamento medicamento quantidade [] = [(medicamento, quantidade)]
-- comprarMedicamento medicamento quantidade (a:as)   | pertencer medicamento (a:as) == False = [(medicamento, quantidade)] ++ (a:as)
--                                                    | medicamento /= fst(a) = comprarMedicamento medicamento quantidade (as)
--                                                    | medicamento == fst(a) = [(medicamento, quantidade + snd(a))] ++ (a:as)

--Questao 1 -> certo
comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento medicamento quantidade [] = [(medicamento, quantidade)]
comprarMedicamento medicamento quantidade (a:as)   | pertencer medicamento (a:as) == False = [(medicamento, quantidade)] ++ (a:as)
                                                   | medicamento == fst(a) = (medicamento, quantidade + snd(a)) : (as)
                                                   | otherwise = a : comprarMedicamento medicamento quantidade (as)


--Questao 2 -> ainda tem um erro mas o corretor nao pegou (ver depois)
tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento medicamento [] = Nothing
tomarMedicamento medicamento (a:as) | pertencer medicamento (a:as) == False = Nothing
                                    | medicamento == fst(a) = Just ((medicamento, snd(a) - 1) : (as))

--Questao 3 -> certo
consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento medicamento [] = 0
consultarMedicamento medicamento (a:as)   | medicamento == fst(a) = snd(a)
                                          | otherwise = consultarMedicamento medicamento (as)

--Questao 4 -> fazendo
demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos = undefined


double :: [Int] -> [Int]
double [] = []
double (a:as) = (a*2) : double (as)


















