import gleeunit/should
import gswarm/shard_manager

pub fn shard_mapping_test() {
  // Same ID always goes to same shard
  let s1 = shard_manager.get_shard_id("market_1", 4)
  let s2 = shard_manager.get_shard_id("market_1", 4)
  s1 |> should.equal(s2)

  // Different IDs (likely) go to different shards
  let s3 = shard_manager.get_shard_id("market_2", 4)
  // Note: Collision is possible but for these strings they should differ
  // We just want to check they are within bounds
  { s1 >= 0 && s1 < 4 } |> should.be_true
  { s3 >= 0 && s3 < 4 } |> should.be_true
}

pub fn single_shard_test() {
  shard_manager.get_shard_id("any", 1) |> should.equal(0)
  shard_manager.get_shard_id("any", 0) |> should.equal(0)
}
