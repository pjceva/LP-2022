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

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento medicamento quantidade [] = [(medicamento, quantidade)]


