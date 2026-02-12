import gleam/otp/actor
import gleam/option.{Some}
import gleam/string
import gleamdb
import gleamdb/storage

pub type NodeRole {
  Leader
  Follower
}

pub type NodeContext {
  NodeContext(
    role: NodeRole,
    db: gleamdb.Db,
    id: String
  )
}

pub fn start(role: NodeRole, cluster_id: String) -> Result(NodeContext, String) {
  case role {
    Leader -> {
      // Debugging: Fallback to start_named to isolate timeout issue
      case gleamdb.start_named(cluster_id, Some(storage.ephemeral())) {
        Ok(db) -> Ok(NodeContext(Leader, db, cluster_id))
        Error(e) -> Error("Failed to start leader: " <> string_error(e))
      }
    }
    Follower -> {
      // Followers connect to the existing fabric
      case gleamdb.connect(cluster_id) {
        Ok(db) -> Ok(NodeContext(Follower, db, cluster_id))
        Error(e) -> Error("Failed to connect follower: " <> e)
      }
    }
  }
}

fn string_error(err: actor.StartError) -> String {
  case err {
    actor.InitTimeout -> "Init Timeout"
    actor.InitExited(reason) -> "Init Exited: " <> string.inspect(reason)
    actor.InitFailed(reason) -> "Init Failed: " <> string.inspect(reason)
  }
}
