## Suppress R CMD check NOTE about no-visible-binding for data.table NSE.
utils::globalVariables(c(
  ".", "Color", "Count", "Decision", "Label", "Truth",
  "color", "comparison", "cue", "delta", "exit_node",
  "family", "hjust", "idx", "inner_bacc", "is_correct",
  "item_id", "kind", "label", "metric", "outcome",
  "picked", "shape", "test_pred", "test_y", "tooltip",
  "value", "x", "xend", "y", "yend"
))
