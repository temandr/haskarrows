import qualified Data.Map.Strict as Map
-- Graph generator in Haskell using Arrows

data Node n = N n | EmptyNode
  deriving (Show)
data Edge e a b = E e (Node a) (Node b) | EmptyEdge
  deriving (Show)
data Graph e v = G e v
  deriving (Show)
data GraphArrow g g' = GA g g'

class Arrow a where   
    arr   :: (b -> c) -> a b c
    --(>>>) :: a b c -> a c d -> a b d

instance Arrow GraphArrow where
--    arr (a -> b) = GA a b
    --arr f = GA f
    --(>>>) (GA xxb@(xb:xsb) xxc@(xc:xsc)) (GA xxd@(xd:xsd) xxe@(xe:xse)) = G [] []

createGraphLift (G e v) = (\(G e v) -> G e $ create_edge e)

create_edge xx = Map.elems $ Map.fromList $ create_edge_helper xx []
create_edge_helper [] nxx = nxx
create_edge_helper ((E e a b) : xs) nxx = create_edge_helper xs $ Map.toList $ test a b
                                            where test EmptyNode EmptyNode = dnxx
                                                  test EmptyNode b@(N bn) = Map.insert bn b dnxx
                                                  test a@(N an) EmptyNode = Map.insert an a dnxx
                                                  test a@(N an) b@(N bn) = Map.insert bn b $ Map.insert an a dnxx
                                                  dnxx = Map.fromList nxx
create_edge_helper ((EmptyEdge) : xs) nxx = create_edge_helper xs nxx

dummy_edges = [(E 0 (N 1) (N 2)), (E 1 (N 1) (N 3)), (E 2 (N 2) (N 3)), (E 3 (EmptyNode) (N 1)), (E 4 (N 3) (EmptyNode)), EmptyEdge]
dummy_graph = G dummy_edges []
