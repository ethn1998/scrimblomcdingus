func isBipartite(n int, edges[][]int) bool { //Function to check if graph is bipartite
    colors := make([]int,n) //Array to track node colors. Color the nodes with \pm 1.
    for i,v := range(colors){
        if v == 0 { //default value 0 means colorless.
            colors[i] = 1 //Fix color of first found node.
            queue := []int{i} //Queue to iteratively color connected nodes in order
            for j := 0; j < len(queue); j++{
                for _,edge := range(edges) {
                    n0 := edge[0]-1 //Array is zero-indexed but problem is one-indexed
                    n1 := edge[1]-1 //Index correction
                    if n0 == queue[j] {
                        if colors[n1] == 0 { //If new node is not colored
                            colors[n1] = -colors[n0] //Color opposite color to parent node
                            queue = append(queue,n1) //Add to queue
                        } else if colors[n1] == colors[n0] { //If we already colored this node before
                            return false //Adjecent nodes cannot have same color
                        }
                    } else if n1 == queue[j] {
                        if colors[n0] == 0 { //If new node is not colored
                            colors[n0] = -colors[n1] //Color opposite color to parent node
                            queue = append(queue,n0) //Add to queue
                        } else if colors[n0] == colors[n1] { //If we already colored this node before
                            return false //Adjecent nodes cannot have same color
                        }
                    }
                }
            }
        }
    }
    return true
}
