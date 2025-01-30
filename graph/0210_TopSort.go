func findOrder(numCourses int, prerequisites [][]int) []int {
    /*
    TOP sort using Kahn's Algorithm, original implementation copied from 0207_Course Schedule
    NOTE: A 'reversed' Kahn's Algorithm can also be defined in terms of incoming edges rather than outgoing edges
    */
    nEdges := make([]int,numCourses) //Track number of incoming edges.
    for _,edge := range(prerequisites){
        nEdges[edge[0]]++
    }
    rootQueue := make([]int,0)
    for i,v := range(nEdges){
        if v == 0 {
            rootQueue = append(rootQueue,i)
        }
    }
    //Statically typed languages like go might be unsuitable for this kind of algorithm.
    for j := 0; j < len(rootQueue); j++ {
        root := rootQueue[j] //Focus on jth element of queue
        for _,prerequisite := range(prerequisites){
            src := prerequisite[1]
            dst := prerequisite[0]
            if src == root {
                nEdges[dst]--
                if nEdges[dst] == 0{
                    rootQueue = append(rootQueue,dst)
                }
            }
        }
    }
    if len(rootQueue) == numCourses{
        /*Reverse topologically sorted array.
        for i, j := 0, len(leafQueue)-1; i < j; i, j = i+1, j-1 {
            leafQueue[i], leafQueue[j] = leafQueue[j], leafQueue[i]
        }
        */
        //No need to reverse ordering if defined in terms of incoming edges
        return rootQueue
    }
    return make([]int,0)
}
