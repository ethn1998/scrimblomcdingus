import "sort"

func eventualSafeNodes(graph [][]int) []int {
    //NOTE: Based on phrasing of question, nodes forming limit cycles are not safe nodes.
    //IDEA: Try a depth first search?????
    //IDEA2: Kahn's Algorithm to filter safe nodes
    //REMARK: TERMINAL NODES ARE ALSO SAFE NODES.
    graphMat := make(map[int](map[int]bool)) //Reformat graph (probably redundant)
    nEdges := make([]int,len(graph)) //Number of incoming edges
    safeNodes := make([]int,0)
    for src,edges := range(graph){
        if len(edges) == 0{
            safeNodes = append(safeNodes,src)
        }else{
            //Reverse graph for future efficiency!?
            for _,dst := range(edges){
                _,ok := graphMat[dst]
                if !ok{
                    graphMat[dst] = make(map[int]bool)
                }
                graphMat[dst][src] = true
                nEdges[src]++
            }
        }
    }
    for i := 0; i < len(safeNodes); i++{
        dst := safeNodes[i]
        for src := range(graphMat[dst]){
            nEdges[src]--
            if nEdges[src] == 0{
                safeNodes = append(safeNodes,src)
            }
        }
    }
    sort.Slice(safeNodes,func(i, j int) bool {
        return safeNodes[i] < safeNodes[j]})
    return safeNodes
}
