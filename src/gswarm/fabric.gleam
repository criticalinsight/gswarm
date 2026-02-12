import gswarm/node.{type NodeContext, Leader, Follower}

pub fn join_fabric(role: node.NodeRole, cluster_id: String) -> Result(NodeContext, String) {
  // In a real-world scenario, this would use DNS or a seed list.
  // For the reference implementation, we rely on the Erlang global registry
  // which is abstracted away by GleamDB's distributed start.
  
  node.start(role, cluster_id)
}

pub fn broadcast_ping(ctx: NodeContext) -> Nil {
  // A simple liveness check across the mesh
  case ctx.role {
    Leader -> Nil // Leader doesn't ping, it rules.
    Follower -> {
      // Future: Implement keepalive to leader
      Nil
    }
  }
}
