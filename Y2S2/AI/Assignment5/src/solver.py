from enum import Enum
from typing import Dict, Optional, Tuple

class Val(Enum):
  NVVB = 0
  NVB = 1
  NB = 2
  N = 3
  Z = 4
  P = 5
  PB = 6
  PVB = 7
  PVVB = 8

ℝ = float
Range = Tuple[Optional[ℝ], ℝ, Optional[ℝ]]
Ranges = Dict[Val, Range]

θ_ranges: Ranges = {
  Val.NVB: (None, -40, -25),
  Val.NB: (-40, -25, -10),
  Val.N: (-20, -10, 0),
  Val.Z: (-5, 0, 5),
  Val.P: (0, 10, 20),
  Val.PB: (10, 25, 40),
  Val.PVB: (25, 40, None)
}

ω_ranges: Ranges = {
  Val.NB: (None, -8, -3),
  Val.N: (-6, -3, 0),
  Val.Z: (-1, 0, 1),
  Val.P: (0, 3, 6),
  Val.PB: (3, 8, None)
}

f_ranges: Ranges = {
  Val.NVVB: (None, -32, -24),
  Val.NVB: (-32, -24, -16),
  Val.NB: (-24, -16, -8),
  Val.N: (-16, -8, 0),
  Val.Z: (-4, 0, 4),
  Val.P: (0, 8, 16),
  Val.PB: (8, 16, 24),
  Val.PVB: (16, 24, 32),
  Val.PVVB: (24, 32, None)
}

b_vals = { key: value[1] for key, value in f_ranges.items() }

fuzzy_table: Dict[Val, Dict[Val, Val]] = {
  Val.NB: {
    Val.NB: Val.NVVB,
    Val.N: Val.NVB,
    Val.Z: Val.NB,
    Val.P: Val.N,
    Val.PB: Val.Z
  },
  Val.N: {
    Val.NB: Val.NVB,
    Val.N: Val.NB,
    Val.Z: Val.N,
    Val.P: Val.Z,
    Val.PB: Val.P
  },
  Val.Z: {
    Val.NB: Val.NB,
    Val.N: Val.N,
    Val.Z: Val.Z,
    Val.P: Val.P,
    Val.PB: Val.PB
  },
  Val.P: {
    Val.NB: Val.N,
    Val.N: Val.Z,
    Val.Z: Val.P,
    Val.P: Val.PB,
    Val.PB: Val.PVB
  },
  Val.PB: {
    Val.NB: Val.Z,
    Val.N: Val.P,
    Val.Z: Val.PB,
    Val.P: Val.PVB,
    Val.PB: Val.PVVB
  },
  Val.PVB: {
    Val.NB: Val.P,
    Val.N: Val.PB,
    Val.Z: Val.PVB,
    Val.P: Val.PVVB,
    Val.PB: Val.PVVB
  },
  Val.NVB: {
    Val.N: Val.NVVB,
    Val.Z: Val.NVB,
    Val.P: Val.NB,
    Val.PB: Val.N,
    Val.NB: Val.NVVB
  }
}

def fuzz(x: ℝ, left: Optional[ℝ], mid: ℝ, right: Optional[ℝ]) -> ℝ:
  if left is not None and left <= x < mid:
    return (x - left) / (mid - left)
  elif right is not None and mid <= x < right:
    return (right - x) / (right - mid)
  elif left is None and x <= mid or right is None and x >= mid:
    return 1
  else:
    return 0

def compute_vals(val: ℝ, ranges: Ranges) -> Dict[Val, ℝ]:
  return { key: fuzz(val, *range) for key, range in ranges.items() }

def solver(θ: ℝ, ω: ℝ) -> Optional[ℝ]:
  θ_vals = compute_vals(θ, θ_ranges)
  ω_vals = compute_vals(ω, ω_ranges)
  f_vals: Dict[Val, ℝ] = { }

  for θ_key, fuzzy_dict in fuzzy_table.items():
    for ω_key, f_val in fuzzy_dict.items():
      val = min(θ_vals[θ_key], ω_vals[ω_key])

      if f_val not in f_vals:
        f_vals[f_val] = val
      else:
        f_vals[f_val] = max(val, f_vals[f_val])

  s = sum(f_vals.values())

  if s == 0:
    return None
  else:
    return sum(f_val * b_vals[f_set] for f_set, f_val in f_vals.items()) / s
