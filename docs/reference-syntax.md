Reference
---------

  - Operators (ordered by precedence: high to low)

    | Operator                  | Syntax               |
    | ------------------------- | -------------------- |
    | factorial                 | `!`                  |
    | square, cube, ...         | `²`, `³`, `⁻¹`, ...  |
    | exponentiation            | `^`, `**`            |
    | multiplication (implicit) | *whitespace*         |
    | modulo                    | `%`                  |
    | division                  | `per`                |
    | division                  | `/`, `÷`             |
    | multiplication (explicit) | `*`, `·`, `×`        |
    | subtraction               | `-`                  |
    | addition                  | `+`                  |
    | unit conversion           | `->`, `→`, `➞`, `to` |
    | assignment                | `=`                  |

    Note that *implicit* multiplication has a higher precedence than division, i.e. `50 cm / 2 m` will be parsed as `50 cm / (2 m)`.

  - Commands

    | Command                  | Syntax             |
    | ------------------------ | ------------------ |
    | help text                | `help`, `?`        |
    | list of variables        | `list`, `ls`, `ll` |
    | reset environment        | `reset`            |
    | clear screen             | `clear`, `cls`     |
    | copy result to clipboard | `copy`, `cp`       |
    | quit (CLI)               | `quit`, `exit`     |
