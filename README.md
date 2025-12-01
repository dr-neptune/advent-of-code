# Advent of Code 🎄🧊

A yearly pile of snow-crusted puzzles solved mostly in Racket (with occasional detours). Each year lives in its own directory: `2015/`, `2020/`, `2022/`, `2023/`, `2024/`, `2025/`.

## How to run a day
- Enter the Nix dev shell for tools and Racket: `nix develop`
- Run a solution: `racket 2025/day-1.rkt`
- Need input? `fetch-aoc-input` pulls it using your saved session cookie (see `utils.rkt` for helpers).

## House style
- Keep solutions tiny and readable; favor `for` comprehensions, `match`, and threading macros (`~>` / `~>>`).
- Split parts with clear comments; keep parsing near the top.
- Default to pure functions; only drop to mutation when profiling says so.
