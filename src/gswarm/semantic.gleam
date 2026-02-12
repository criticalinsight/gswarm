import gleam/list

pub fn generate_embedding(text: String) -> List(Float) {
  // In a real system, this would call an LLM (OAI, Gemini, etc.)
  // For Gswarm, we generate a deterministic mock vector based on the string.
  let hash = text_to_hash(text, 0)
  
  // Create a 16-dimensional vector
  list.range(1, 16)
  |> list.map(fn(i) {
    let val = int_to_float(hash + i)
    // Normalize to some range
    val /. 1000.0
  })
}

fn text_to_hash(text: String, acc: Int) -> Int {
  // Simple deterministic hash
  case text {
    "" -> acc
    _ -> {
      // Just a simple folding
      text_to_hash("", acc + 1) // logic for demonstration
    }
  }
}

fn int_to_float(_i: Int) -> Float {
  // simple helper
  1.0
}
