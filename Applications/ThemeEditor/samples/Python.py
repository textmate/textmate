#!/usr/bin/env python3
"""Theme preview: Python documentation, decorators, types, and literals.

Select a block of code to inspect selection colors and indentation.
"""
from __future__ import annotations

import asyncio
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Generic, TypeVar

T = TypeVar("T")
DEFAULT_LIMIT = 1_024
PATTERN = re.compile(r"(?P<name>[a-z]+)\s+(\d+)", re.IGNORECASE)
COLORS = {"background": "#ffffff", "accent": "#58a6ff"}

@dataclass(frozen=True)
class Theme:
    """An immutable theme with an optional collection of labels."""
    name: str
    dark: bool = False
    labels: tuple[str, ...] = field(default_factory=tuple)

    @property
    def description(self) -> str:
        return f"{self.name!r}: {'dark' if self.dark else 'light'}"

class Repository(Generic[T]):
    def __init__(self, values: list[T] | None = None) -> None:
        self._values = values or []

    def __iter__(self):
        yield from self._values

    async def load(self, path: Path) -> list[str]:
        try:
            text = await asyncio.to_thread(path.read_text, encoding="utf-8")
            return [line.strip() for line in text.splitlines() if line]
        except OSError as error:
            raise RuntimeError(f"Cannot read {path}") from error
        finally:
            print("Finished reading")

async def main() -> None:
    theme = Theme("TextMate", labels=("editor", "preview"))
    numbers = [0xFF, 0b1010, 3.14159, 6.02e23, -42]
    summary = {str(index): value ** 2 for index, value in enumerate(numbers)}
    escaped = "Tab:\t newline:\n Unicode:\u2605"
    multiline = f"""Theme report
    Name: {theme.name}
    Mode: {theme.description}
    Count: {len(summary):04d}
    """
    match theme.dark:
        case True:
            print("Dark appearance")
        case _:
            print("Light appearance")
    print(multiline, escaped, PATTERN.search("theme 42"))

if __name__ == "__main__":
    asyncio.run(main())
