import qualified Data.Map.Strict as Map
-- Graph generator in Haskell using Arrows

data Node n = N n | EmptyNode
  deriving (Show)
data Edge e a b = E e (Node a) (Node b) | EmptyEdge
  deriving (Show)
newtype Graph e v = Graph {runGraph :: e -> v}

class Arrow a where   
    arr   :: (b -> c) -> a b c
    (>>>) :: a b c -> a c d -> a b d
    (***) :: a b c -> a b' c' -> a (b,b') (c,c')

instance Arrow Graph where
    arr f = Graph f
    (>>>) (Graph f) (Graph g) = Graph $ flip (.) f g
    (***) (Graph f) (Graph g) = Graph $ (\(x,y) -> (f x, g y))

-- createGraphLift (G e v) = (\(G e v) -> G e $ create_edge e)

create_edge :: Ord k => [Edge t k k] -> [Node k]
create_edge xx = Map.elems $ Map.fromList $ create_edge_helper xx []
create_edge_helper [] nxx = nxx
create_edge_helper ((E e a b) : xs) nxx = create_edge_helper xs $ Map.toList $ test a b
                                            where test EmptyNode EmptyNode = dnxx
                                                  test EmptyNode b@(N bn) = Map.insert bn b dnxx
                                                  test a@(N an) EmptyNode = Map.insert an a dnxx
                                                  test a@(N an) b@(N bn) = Map.insert bn b $ Map.insert an a dnxx
                                                  dnxx = Map.fromList nxx
create_edge_helper ((EmptyEdge) : xs) nxx = create_edge_helper xs nxx

getSharedNodes :: (Ord k) => ([Node k], [Node k]) -> [Node k]
getSharedNodes (ff, gg) = Map.elems $ Map.fromList $ map (\xx@(N x) -> (x,xx)) $ getSharedNodes_helper ff gg

getSharedNodes_helper :: (Ord k) => [Node k] -> [Node k] -> [Node k]
getSharedNodes_helper ff [] = ff
getSharedNodes_helper ff (x:xs) = x : getSharedNodes_helper ff (xs)

get_nodes xx = get_nodes_helper xx []
get_nodes_helper [] nxx = nxx
get_nodes_helper (EmptyNode : xs) nxx = get_nodes_helper xs nxx
get_nodes_helper ((N n) : xs) nxx = get_nodes_helper xs (n : nxx)

-- use example: runGraph comp_example dummy_edges1
comp_example :: (Arrow a, Ord k) => a [Edge t k k] [k]
comp_example = (arr create_edge) >>> (arr get_nodes)

-- use example: runGraph addGraphEdges (dummy_edges1, dummy_edges2)
addGraphsEdges :: (Arrow a, Ord k) => a ([Edge t k k], [Edge t k k]) [Node k]
addGraphsEdges = ((arr create_edge) *** (arr create_edge)) >>> (arr getSharedNodes)

dummy_edges1 = [(E 0 (N 1) (N 2)), (E 1 (N 1) (N 3)), (E 2 (N 2) (N 3)), (E 3 (EmptyNode) (N 1)), (E 4 (N 3) (EmptyNode)), EmptyEdge]

dummy_edges2 = [(E 0 (N 3) (N 2)), (E 1 (N 2) (N 4)), (E 2 (N 4) EmptyNode)]
