signature GraphLib =
sig
  type 'a node
  type 'a graph
  type distVal
  val makeNode : 'a * 'a * int -> 'a node
  val nodeToTuple : 'a node -> 'a * 'a * int
  val bfs : 'a graph -> 'a -> ('a * 'a -> bool) -> 'a list
  val dfs : 'a graph -> 'a -> ('a * 'a -> bool) -> 'a list
  val dijkstra : 'a graph -> 'a -> ('a * 'a -> bool) -> (('a * distVal) list)
  val topologicalSort : 'a graph -> ('a * 'a -> bool) -> 'a list
  val hasCycle : 'a graph -> ('a * 'a -> bool) -> bool
  val minimalSpaningTree: 'a graph -> ('a * 'a -> bool) -> 'a node list
end
structure GraphLib : GraphLib =
struct
    datatype 'a node = Node of ('a * 'a * int)
    type 'a graph = ('a node list)
    datatype distVal = FINITE of int | INFINITY

    (* utilities *)
    fun exists _ [] = false
    | exists func (y::rest) = if func y then true else exists func rest

    fun foldr _ init [] = init
    | foldr func acc (x::xs) = func (x,(foldr func acc xs))

    fun foldl _ init [] = init
    | foldl func acc (x::xs) = func (x,foldl func acc xs)

    fun filter _ [] = []
    | filter func (x::xs) = if func x then x :: filter func xs else filter func xs

    fun map _ [] = []
    | map func (x::xs) = (func x) :: (map func xs)

    fun makeNode (x,y,w) = Node(x,y,w)
    fun nodeToTuple (Node(x,y,w)) = (x,y,w)
    (* functions *)
    fun bfs (graph: 'a graph) start cmpFunc =
    let
        fun neighbors v =
            foldr (fn (Node(x,y,_), acc) =>
            if cmpFunc(x,v) then y :: acc else acc
            ) [] graph

        fun loop ([], visited) = visited
            | loop (v::queue, visited) =
                if exists (fn x => cmpFunc(x,v)) visited then
                loop (queue, visited)
                else
                let
                    val neighborsList = neighbors v
                    val newNodes =
                    filter (fn x =>
                        not (exists (fn y => cmpFunc(y,x)) visited)
                    ) neighborsList
                in
                    loop (queue @ newNodes, visited @ [v])
                end
    in
        loop ([start], [])
    end

    fun dfs (graph: 'a graph) start cmpFunc=
    let
        fun neighbors v =
            foldr (fn (Node(x,y,_), acc) =>
            if cmpFunc(x,v) then y :: acc else acc
            ) [] graph
        
        fun loop (current,visited) =
        let
            val neighborsList = neighbors current
            fun isNewValue x v = not (exists (fn y => cmpFunc(y,x)) v)
            val newNodes =
                    filter (fn x =>
                        isNewValue x visited
                    ) neighborsList
            fun processNodes [] v = v
            | processNodes (x::xs) v =
                let
                val newVal = isNewValue x v
                in
                if newVal then processNodes xs (loop (x,v)) else processNodes xs v
                end
        in
            processNodes newNodes (visited@[current])
        end
    in
    loop (start,[])
    end

    fun hasCycle graph cmpFunc =
    let
        val allNodes =
        foldr (fn (Node(x,y,_), acc) =>
            let
            fun addSmart n lst = if exists (fn m => cmpFunc(m,n)) lst then lst else n::lst
            in
            addSmart x (addSmart y acc)
            end
        ) [] graph

        fun dfsCycle (current, visited, stack) =
        if exists (fn x => cmpFunc(x,current)) stack then
            true
        else if exists (fn x => cmpFunc(x,current)) visited then
            false
        else
            let
            val newVisited = current :: visited
            val nbrs = foldr (fn (Node(x,y,_), acc) =>
                        if cmpFunc(x,current) then y::acc else acc
                        ) [] graph
            val result =
                foldl (fn (nbr, accFlag) =>
                        accFlag orelse dfsCycle(nbr, newVisited, current::stack)
                    ) false nbrs
            in
            result
            end

        fun run [] _ = false
        | run (n::ns) visited =
            if exists (fn x => cmpFunc(x,n)) visited then
                run ns visited
            else
                let
                val hasC = dfsCycle (n, visited, [])
                in
                if hasC then true else run ns (n::visited)
                end
    in
        run allNodes []
    end

    fun topologicalSort (graph: 'a graph) cmpFunc =
    let
        fun allNodes g =
        foldr (fn (Node(x,y,_), acc) =>
            let
            fun addSmart n lst = if exists (fn m => cmpFunc(m,n)) lst then lst else n::lst
            in
            addSmart x (addSmart y acc)
            end
        ) [] g
        
        fun remove _ [] = []
        | remove x (y::xs) = if cmpFunc(y,x) then remove x xs else y::(remove x xs)
        
        fun nodesLeft [] result = result
        | nodesLeft (Node(x,y,_)::rest) result = 
            nodesLeft rest (remove y result)

        fun removeGraph [] _ newGraph = newGraph
        | removeGraph (Node(x,y,t)::rest) invalid newGraph =
            let
            val xIn = exists (fn z => cmpFunc(x,z)) invalid
            val yIn = exists (fn z => cmpFunc(y,z)) invalid
            in
            if xIn orelse yIn then
                removeGraph rest invalid newGraph
            else
                removeGraph rest invalid (Node(x,y,t)::newGraph)
            end
        fun loop [] result = result
        | loop remains result =
        let
            val nextToAdd = nodesLeft remains (allNodes remains)
            val nextSort = result@nextToAdd
            val nextRemains = removeGraph remains nextToAdd []
        in
            loop nextRemains nextSort
        end
    
    in
        if hasCycle graph cmpFunc then []
        else 
        let
            val found = loop graph []
            val nodes = allNodes graph
            fun add [] res = res
            | add (x::xs) res =
                let
                val xIn = exists (fn y => cmpFunc(y,x)) res
                in
                if xIn then add xs res else add xs (res@[x])
                end
        in
        add nodes []
        end
    end

    fun dijkstra (graph: 'a graph) start cmpFunc =
    let
    fun neighbors v =
            foldr (fn (Node(x,y,_), acc) =>
            if cmpFunc(x,v) then y :: acc else acc
            ) [] graph

    val nodes =
        foldr
        (fn (Node(x,y,_),acc) =>
            let
            fun addSmart newVal currentList = if exists (fn m => cmpFunc(m,newVal)) currentList then currentList 
                                                                                                else newVal :: currentList
            in
            addSmart x (addSmart y acc)
            end
        ) [] graph

    (* distance map helper *)
    val distanceMapStart = map (fn t => if cmpFunc(t,start) then (t,FINITE(0)) else (t,INFINITY)) nodes
    fun getDist node [] cmpFunc = INFINITY
        | getDist node ((x,y)::xs) cmpFunc =
        if cmpFunc(x,node) then y else getDist node xs cmpFunc
    fun setDist node newDist distTable cmpFunc =
        map (fn (n,d) => if cmpFunc(n,node) then (n,newDist) else (n,d)) distTable
    
    fun loop [] _ distTable = distTable
        | loop unvisited visited distTable =
            let
                fun pickMin [] = raise Fail "Not possible"
                | pickMin (n::ns) =
                    List.foldl (fn (x, best) =>
                        case (getDist x distTable cmpFunc, getDist best distTable cmpFunc) of
                            (FINITE(d1), FINITE(d2)) => if d1 < d2 then x else best
                            | (FINITE(_), INFINITY) => x
                            | (_, _) => best
                    ) n ns

                val current = pickMin unvisited
                
                val newVisited = current :: visited
                val newUnvisited = filter (fn n => not (cmpFunc(n,current))) unvisited

                val neighborsWithWeights =
                    foldr (fn (Node(x,y,w), acc) =>
                        if cmpFunc(x,current) then (y,w)::acc else acc
                    ) [] graph

                val updatedDistTable =
                    foldl (fn ((nbr,weight), dt) =>
                        case getDist current dt cmpFunc of
                            INFINITY => dt  (* shouldn't happen *)
                        | FINITE(currDist) =>
                            case getDist nbr dt cmpFunc of
                                INFINITY => setDist nbr (FINITE(currDist + weight)) dt cmpFunc
                                | FINITE(prevDist) =>
                                    if currDist + weight < prevDist then
                                    setDist nbr (FINITE(currDist + weight)) dt cmpFunc
                                    else dt
                    ) distTable neighborsWithWeights
            in
                loop newUnvisited newVisited updatedDistTable
            end
    in
    loop nodes [] distanceMapStart
    end

    fun minimalSpaningTree (graph: 'a graph) cmpFunc =
    let
        val nodes =
            foldr (fn (Node(x,y,_),acc) =>
                let
                    fun addSmart n lst =
                        if exists (fn m => cmpFunc(m,n)) lst then lst else n::lst
                in
                    addSmart x (addSmart y acc)
                end
            ) [] graph

        fun insertSorted edge [] = [edge]
          | insertSorted (Node(x,y,w)) (Node(x2,y2,w2)::xs) =
                if w <= w2 then Node(x,y,w)::Node(x2,y2,w2)::xs
                else Node(x2,y2,w2)::insertSorted (Node(x,y,w)) xs

        val sortedEdges = foldl (fn (elem,acc) => insertSorted elem acc) [] graph

        fun find x parents =
            case List.find (fn (n,p) => cmpFunc(n,x)) parents of
                SOME (_,p) => if cmpFunc(p,x) then x else find p parents
            | NONE => x

        fun uniteParents (x,y) parents =
            let
                val rootX = find x parents
                val rootY = find y parents
            in
                if cmpFunc(rootX, rootY) then parents
                else
                    map (fn (n,p) =>
                        if cmpFunc(n,rootX) then (n,rootY) else (n,p)
                    ) parents
            end

        val parents = map (fn n => (n,n)) nodes

        fun loop [] _ mst = mst
          | loop (Node(x,y,w)::es) parents mst =
                let
                    val rootX = find x parents
                    val rootY = find y parents
                in
                    if cmpFunc(rootX, rootY) then
                        loop es parents mst
                    else
                        loop es (uniteParents (x,y) parents) (Node(x,y,w)::mst)
                end

        val mstEdges = loop sortedEdges parents []
    in
      mstEdges
    end
end
