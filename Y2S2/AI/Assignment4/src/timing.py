from functools import wraps
from os import getenv
from time import time
from typing import Any, Callable, Dict, List, TypeVar, cast

timing_disabled = getenv("TIMING") == "off"

F = TypeVar("F", bound = Callable[..., Any])

def timed_impl(name: str, f: F) -> F:
  if timing_disabled:
    return f

  @wraps(f)
  def wrapped(*args: Any, **kw: Any) -> Any:
    t0 = time()
    result = f(*args, **kw)
    t1 = time()

    print(f"{name} took {t1 - t0}s.")
    return result

  return cast(F, wrapped)

def timed(name: str) -> Callable[[F], F]:
  return lambda f: timed_impl(name, f)
