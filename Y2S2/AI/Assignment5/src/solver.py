from enum import Enum
from typing import Dict, Optional, Tuple

class Set(Enum):
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
Ranges = Dict[Set, Range]
Degrees = Dict[Set, ℝ]

θ_ranges: Ranges = {
  Set.NVB: (None, -40, -25),
  Set.NB: (-40, -25, -10),
  Set.N: (-20, -10, 0),
  Set.Z: (-5, 0, 5),
  Set.P: (0, 10, 20),
  Set.PB: (10, 25, 40),
  Set.PVB: (25, 40, None)
}

ω_ranges: Ranges = {
  Set.NB: (None, -8, -3),
  Set.N: (-6, -3, 0),
  Set.Z: (-1, 0, 1),
  Set.P: (0, 3, 6),
  Set.PB: (3, 8, None)
}

f_ranges: Ranges = {
  Set.NVVB: (None, -32, -24),
  Set.NVB: (-32, -24, -16),
  Set.NB: (-24, -16, -8),
  Set.N: (-16, -8, 0),
  Set.Z: (-4, 0, 4),
  Set.P: (0, 8, 16),
  Set.PB: (8, 16, 24),
  Set.PVB: (16, 24, 32),
  Set.PVVB: (24, 32, None)
}

b_vals = { key: value[1] for key, value in f_ranges.items() }

fuzzy_table: Dict[Set, Dict[Set, Set]] = {
  Set.NVB: {
    Set.NB: Set.NVVB,
    Set.N: Set.NVVB,
    Set.Z: Set.NVB,
    Set.P: Set.NB,
    Set.PB: Set.N
  },
  Set.NB: {
    Set.NB: Set.NVVB,
    Set.N: Set.NVB,
    Set.Z: Set.NB,
    Set.P: Set.N,
    Set.PB: Set.Z
  },
  Set.N: {
    Set.NB: Set.NVB,
    Set.N: Set.NB,
    Set.Z: Set.N,
    Set.P: Set.Z,
    Set.PB: Set.P
  },
  Set.Z: {
    Set.NB: Set.NB,
    Set.N: Set.N,
    Set.Z: Set.Z,
    Set.P: Set.P,
    Set.PB: Set.PB
  },
  Set.P: {
    Set.NB: Set.N,
    Set.N: Set.Z,
    Set.Z: Set.P,
    Set.P: Set.PB,
    Set.PB: Set.PVB
  },
  Set.PB: {
    Set.NB: Set.Z,
    Set.N: Set.P,
    Set.Z: Set.PB,
    Set.P: Set.PVB,
    Set.PB: Set.PVVB
  },
  Set.PVB: {
    Set.NB: Set.P,
    Set.N: Set.PB,
    Set.Z: Set.PVB,
    Set.P: Set.PVVB,
    Set.PB: Set.PVVB
  }
}

def membership_degree(val: ℝ, low: Optional[ℝ], mid: ℝ, high: Optional[ℝ]) -> ℝ:
  if low is not None and low <= val < mid:
    return (val - low) / (mid - low)
  elif high is not None and mid <= val < high:
    return (high - val) / (high - mid)
  elif low is None and val <= mid or high is None and val >= mid:
    return 1
  else:
    return 0

def membership_degrees(val: ℝ, ranges: Ranges) -> Degrees:
  return { key: membership_degree(val, *range) for key, range in ranges.items() }

def f_membership_degrees(μ_θ: Degrees, μ_ω: Degrees) -> Degrees:
  μ_f = dict.fromkeys(Set, 0.0)

  for θ_key, θ_dict in fuzzy_table.items():
    for ω_key, f_set in θ_dict.items():
      degree = min(μ_θ[θ_key], μ_ω[ω_key])
      μ_f[f_set] = max(degree, μ_f[f_set])

  return μ_f

def defuzzify(μ: Degrees) -> Optional[ℝ]:
  s = sum(μ.values())
  if s == 0:
    return None

  b_sum = sum(f_val * b_vals[f_set] for f_set, f_val in μ.items())
  return b_sum / s

def solver(θ: ℝ, ω: ℝ) -> Optional[ℝ]:
  μ_θ = membership_degrees(θ, θ_ranges)
  μ_ω = membership_degrees(ω, ω_ranges)

  μ_f = f_membership_degrees(μ_θ, μ_ω)
  return defuzzify(μ_f)
