func canFinish(numCourses int, prerequisites [][]int) bool {
    /*
    We can finish all courses if the graph formed by prerequisites is a Directed Acyclic Graph (DAG).
    Idea: Implement "Kahn's Algorithm" here to check if graph is a DAG.
    */
    nEdges := make([]int,numCourses) //Track number of outgoing edges.
    for _,edge := range(prerequisites){
        nEdges[edge[1]]++
    }
    leafQueue := make([]int,0)
    //leaves := make(map[int]bool) //Redundancy to make sure that node has been accounted for robustness.
    for i,v := range(nEdges){
        if v == 0 {
            leafQueue = append(leafQueue,i)
            //leaves[i] = true
        }
    }
    //Statically typed languages like go might be unsuitable for this kind of algorithm.
    for j := 0; j < len(leafQueue); j++ {
        leaf := leafQueue[j] //Focus on jth element of queue
        for _,prerequisite := range(prerequisites){
            src := prerequisite[1]
            dst := prerequisite[0]
            if dst == leaf {
                nEdges[src]--
                if nEdges[src] == 0{
                    leafQueue = append(leafQueue,src)                    
                }
            }
        }
    }
    return len(leafQueue) == numCourses
}
