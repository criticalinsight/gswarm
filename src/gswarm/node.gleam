import gleam/otp/actor
import gleam/option.{Some}
import gleam/string
import gleam/erlang/process
import gleamdb
import gleamdb/storage/mnesia

pub type NodeRole {
  Leader
  Follower
  LeaderEphemeral
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
      // Use Mnesia for Durable Sovereignty
      case gleamdb.start_distributed(cluster_id, Some(mnesia.adapter())) {
        Ok(db) -> Ok(NodeContext(Leader, db, cluster_id))
        Error(e) -> Error("Failed to start leader: " <> string_error(e))
      }
    }
    LeaderEphemeral -> {
      // Ephemeral for benchmarking engine raw capacity
      case gleamdb.start_distributed(cluster_id, option.None) {
        Ok(db) -> Ok(NodeContext(Leader, db, cluster_id))
        Error(e) -> Error("Failed to start ephemeral leader: " <> string_error(e))
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

pub fn promote_to_leader(ctx: NodeContext) -> Result(NodeContext, String) {
  case ctx.role {
    Leader | LeaderEphemeral -> Ok(ctx)
    Follower -> {
      // Autonomous Promotion: Restart node as Leader
      start(Leader, ctx.id)
    }
  }
}

import gleamdb/global

pub fn stop(ctx: NodeContext) {
  let assert Ok(pid) = process.subject_owner(ctx.db)
  process.kill(pid)
  // Clean up global registry
  global.unregister("gleamdb_leader")
  global.unregister("gleamdb_" <> ctx.id)
}

fn string_error(err: actor.StartError) -> String {
  case err {
    actor.InitTimeout -> "Init Timeout"
    actor.InitExited(reason) -> "Init Exited: " <> string.inspect(reason)
    actor.InitFailed(reason) -> "Init Failed: " <> string.inspect(reason)
  }
}
