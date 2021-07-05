
-- import Data.Tree ()
-- data Arbol a = Hoja | Nodo a (Arbol a )(Arbol a) deriving (Show, Eq)
-- arbol = Nodo 9(Nodo 3(Nodo 2(Nodo 5 Hoja Hoja)(Nodo 3 Hoja Hoja))(Nodo 4 Hoja Hoja))(Nodo 7(Nodo 8 Hoja Hoja)(Nodo 9 Hoja Hoja))

-- nNodos :: Arbol a -> Integer
-- nNodos Hoja = 0
-- nNodos (Nodo x i d)= 1+nNodos i + nNodos d

-- profundidad :: Arbol a -> Integer
-- profundidad Hoja = 0
-- profundidad (Nodo x i d) = 1 + max(profundidad i)(profundidad d)

-- preorden :: Arbol a -> [a]
-- preorden Hoja = []
-- preorden (Nodo x i d) = x : (preorden i ++ preorden d)

-- postorden :: Arbol a -> [a]
-- postorden Hoja = []
-- postorden (Nodo x i d) = postorden i ++ postorden d ++ [x]

-- tree = Node "r"[Node "a"[Node"c"], Node "d"[]],Node "b"[Node "e"[],Node "f"[]]

