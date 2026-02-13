import gleam/io
import gleeunit/should
import gleam/int
import gswarm/cms
import gswarm/hll

pub fn cms_test() {
  let sketch = cms.new(100, 3)
  
  // Insert keys with different frequencies
  let sketch = int.range(from: 1, to: 101, with: sketch, run: fn(s, _) {
    cms.increment(s, "key1")
  })
  let sketch = int.range(from: 1, to: 11, with: sketch, run: fn(s, _) {
    cms.increment(s, "warm")
  })
  let sketch = cms.increment(sketch, "cold")
  
  io.println("CMS Key1: " <> int.to_string(cms.estimate(sketch, "key1")))
  io.println("CMS Warm: " <> int.to_string(cms.estimate(sketch, "warm")))
  io.println("CMS Cold: " <> int.to_string(cms.estimate(sketch, "cold")))

  // Test estimates (CMS always overestimates, but should be close)
  { cms.estimate(sketch, "key1") >= 100 } |> should.be_true()
  { cms.estimate(sketch, "warm") >= 10 } |> should.be_true()
  { cms.estimate(sketch, "cold") >= 1 } |> should.be_true()
  cms.estimate(sketch, "unknown") |> should.equal(0)
  
  // Verify overestimation isn't too extreme for this small width
  { cms.estimate(sketch, "warm") <= 20 } |> should.be_true()
}

pub fn hll_test() {
  let card = hll.new(10)
  
  // Insert 50 unique items
  let card = int.range(from: 1, to: 50, with: card, run: fn(c, i) {
    hll.insert(c, "market_" <> int.to_string(i))
  })
  
  // Re-insert same items (should not change cardinality significantly)
  let card = int.range(from: 1, to: 50, with: card, run: fn(c, i) {
    hll.insert(c, "market_" <> int.to_string(i))
  })
  
  let estimate = hll.estimate(card)
  // Ideal estimate is 50. HLL with precision 10 has standard error ~3.25%
  // So it should be within 40-60 range easily.
  { estimate >= 40 } |> should.be_true()
  { estimate <= 60 } |> should.be_true()
}
