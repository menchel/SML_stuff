(*

Simple graph libary in SML

*)
(* an attempt in a much more elegant funtor*)
signature NODE_DATA = sig
    type t
    val eq : t * t -> bool
end
functor GraphLibFn (N : NODE_DATA) =
struct
    datatype node = Node of (N.t * N.t * int)
    type graph = node list
    datatype distVal = FINITE of int | INFINITY

    (* utilities *)
    fun exists _ [] = false
      | exists func (y::rest) = if func y then true else exists func rest

    fun foldr _ init [] = init
      | foldr func acc (x::xs) = func (x, (foldr func acc xs))

    fun foldl _ acc [] = acc
      | foldl func acc (x::xs) = foldl func (func (x, acc)) xs

    fun filter _ [] = []
      | filter func (x::xs) = if func x then x :: filter func xs else filter func xs

    fun map _ [] = []
      | map func (x::xs) = (func x) :: (map func xs)

    (* methods *)
    fun makeNode (x,y,w) = Node(x,y,w)
    fun nodeToTuple (Node(x,y,w)) = (x,y,w)
    fun empty () = []
    fun addEdge graph (x,y,z) = (Node(x,y,z))::graph

    fun removeEdge [] _ = []
      | removeEdge (Node(x,y,z)::xs) (Node(xt,yt,zt)) =
        if N.eq(x,xt) andalso N.eq(y,yt) andalso z=zt 
        then (removeEdge xs (Node(xt,yt,zt)))
        else (Node(x,y,z)::removeEdge xs (Node(xt,yt,zt)))

    fun neighbors [] _ = []
      | neighbors (Node(x,y,z)::xs) node =
      if N.eq(x,node) then y::(neighbors xs node) else (neighbors xs node)

    fun nodes graph =
        foldr (fn (Node(x,y,_), acc) =>
            let
                fun addSmart n lst = if exists (fn m => N.eq(m,n)) lst then lst else n::lst
            in
                addSmart x (addSmart y acc)
            end
        ) [] graph

    fun edges a = a

    (*bfs from a given node*)
    fun bfs (graph: graph) start =
    let
        fun neighbors v =
            foldr (fn (Node(x,y,_), acc) =>
            if N.eq(x,v) then y :: acc else acc
            ) [] graph

        fun loop ([], visited) = visited
          | loop (v::queue, visited) =
                if exists (fn x => N.eq(x,v)) visited then
                loop (queue, visited)
                else
                let
                    val neighborsList = neighbors v
                    val newNodes =
                    filter (fn x =>
                        not (exists (fn y => N.eq(y,x)) visited)
                    ) neighborsList
                in
                    loop (queue @ newNodes, visited @ [v])
                end
    in
        loop ([start], [])
    end

    (* dfs from a given node *)
    fun dfs (graph: graph) start =
    let
        fun neighbors v =
            foldr (fn (Node(x,y,_), acc) =>
            if N.eq(x,v) then y :: acc else acc
            ) [] graph
        
        fun loop (current,visited) =
        let
            val neighborsList = neighbors current
            fun isNewValue x v = not (exists (fn y => N.eq(y,x)) v)
            val newNodes = filter (fn x => isNewValue x visited) neighborsList
            fun processNodes [] v = v
              | processNodes (x::xs) v =
                let val newVal = isNewValue x v
                in if newVal then processNodes xs (loop (x,v)) else processNodes xs v end
        in
            processNodes newNodes (visited@[current])
        end
    in
        loop (start,[])
    end

    (* cycle detection *)
    fun hasCycle graph =
    let
        val allNodes = nodes graph
        fun dfsCycle (current, visited, stack) =
        if exists (fn x => N.eq(x,current)) stack then true
        else if exists (fn x => N.eq(x,current)) visited then false
        else
            let
            val newVisited = current :: visited
            val nbrs = foldr (fn (Node(x,y,_), acc) =>
                        if N.eq(x,current) then y::acc else acc
                        ) [] graph
            in
            foldl (fn (nbr, accFlag) =>
                        accFlag orelse dfsCycle(nbr, newVisited, current::stack)
                    ) false nbrs
            end

        fun run [] _ = false
          | run (n::ns) visited =
            if exists (fn x => N.eq(x,n)) visited then run ns visited
            else
                let val hasC = dfsCycle (n, visited, [])
                in if hasC then true else run ns (n::visited) end
    in
        run allNodes []
    end

    (* topological sort *)
    fun topologicalSort (graph: graph) =
    let
        fun allNodes g = nodes g
        fun remove _ [] = []
          | remove x (y::xs) = if N.eq(y,x) then remove x xs else y::(remove x xs)
        fun nodesLeft [] result = result
          | nodesLeft (Node(x,y,_)::rest) result = nodesLeft rest (remove y result)
        fun removeGraph [] _ newGraph = newGraph
          | removeGraph (Node(x,y,t)::rest) invalid newGraph =
            let
                val xIn = exists (fn z => N.eq(x,z)) invalid
                val yIn = exists (fn z => N.eq(y,z)) invalid
            in
                if xIn orelse yIn then removeGraph rest invalid newGraph
                else removeGraph rest invalid (Node(x,y,t)::newGraph)
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
        if hasCycle graph then []
        else 
        let
            val found = loop graph []
            val ns = allNodes graph
            fun add [] res = res
              | add (x::xs) res =
                if exists (fn y => N.eq(y,x)) res then add xs res else add xs (res@[x])
        in 
            add ns []
        end
    end

    (* dijkstra *)
    fun dijkstra (graph: graph) start =
    let
        val ns = nodes graph
        val distanceMapStart = map (fn t => if N.eq(t,start) then (t,FINITE(0)) else (t,INFINITY)) ns
        fun getDist node [] = INFINITY
          | getDist node ((x,y)::xs) = if N.eq(x,node) then y else getDist node xs
        fun setDist node newDist distTable =
            map (fn (n,d) => if N.eq(n,node) then (n,newDist) else (n,d)) distTable
        
        fun loop [] _ distTable = distTable
          | loop unvisited visited distTable =
            let
                fun pickMin (n::ns) =
                    foldl (fn (x, best) =>
                        case (getDist x distTable, getDist best distTable) of
                            (FINITE(d1), FINITE(d2)) => if d1 < d2 then x else best
                            | (FINITE(_), INFINITY) => x
                            | (_, _) => best
                    ) n ns
                val current = pickMin unvisited
                val newVisited = current :: visited
                val newUnvisited = filter (fn n => not (N.eq(n,current))) unvisited
                val neighborsWithWeights =
                    foldr (fn (Node(x,y,w), acc) =>
                        if N.eq(x,current) then (y,w)::acc else acc
                    ) [] graph
                val updatedDistTable =
                    foldl (fn ((nbr,weight), dt) =>
                        case getDist current dt of
                            INFINITY => dt
                        | FINITE(currDist) =>
                            case getDist nbr dt of
                                INFINITY => setDist nbr (FINITE(currDist + weight)) dt
                                | FINITE(prevDist) =>
                                    if currDist + weight < prevDist 
                                    then setDist nbr (FINITE(currDist + weight)) dt 
                                    else dt
                    ) distTable neighborsWithWeights
            in 
                loop newUnvisited newVisited updatedDistTable 
            end
    in 
        loop ns [] distanceMapStart 
    end

    (* minimum spanning tree *)
    fun minimalSpaningTree (graph: graph) =
    let
        val ns = nodes graph
        fun insertSorted edge [] = [edge]
          | insertSorted (Node(x,y,w)) (Node(x2,y2,w2)::xs) =
                if w <= w2 then Node(x,y,w)::Node(x2,y2,w2)::xs
                else Node(x2,y2,w2)::insertSorted (Node(x,y,w)) xs
        val sortedEdges = foldl (fn (elem,acc) => insertSorted elem acc) [] graph
        fun find x parents =
            case List.find (fn (n,p) => N.eq(n,x)) parents of
                SOME (_,p) => if N.eq(p,x) then x else find p parents
            | NONE => x
        fun uniteParents (x,y) parents =
            let val rootX = find x parents
                val rootY = find y parents
            in if N.eq(rootX, rootY) then parents
               else map (fn (n,p) => if N.eq(n,rootX) then (n,rootY) else (n,p)) parents
            end
        val parents = map (fn n => (n,n)) ns
        fun loop [] _ mst = mst
          | loop (Node(x,y,w)::es) parents mst =
                let val rootX = find x parents
                    val rootY = find y parents
                in if N.eq(rootX, rootY) then loop es parents mst
                   else loop es (uniteParents (x,y) parents) (Node(x,y,w)::mst)
                end
    in 
        loop sortedEdges parents [] 
    end
end

