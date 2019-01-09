Pros and cons
-------------

**Reasons to use Insect**

  - Insect is **open source**.
  - There is a [web version](https://Insect.sh/) that requires **no installation**.
  - With both browser and terminal versions available, insect is truly **cross-platform**.
  - Insect has *first-class* support for **physical units**, including metric and binary prefixes.
    While evaluating your calculation, Insect ensures that you did not accidentally make any
    mistakes in combining the physical quantities.
  - Insect supports an [**interactive**](https://en.wikipedia.org/wiki/REPL) style with its
    readline-based interface. There is a saved history that can be browsed by pressing up- and
    down keys. The history is also searchable via *Ctrl-R*.
  - The syntax of Insect is rather strict. The parser does not try to be "smart" on syntactically
    incorrect input, so there shouldn't be any surprises - and you can trust the result of your
    calculation. The parsed user input is always pretty-printed for a quick double-check.
  - Insect is written in [PureScript](http://www.purescript.org/) and therefore benefits from
    all the safety-guarantees that a strictly-typed functional programming language gives you.
  - The source code of [purescript-quantities](https://github.com/sharkdp/purescript-quantities)
    (the underlying library for physical units) as well as the code of Insect itself is
    **extensively tested**.

**Reasons to choose an alternative**

  - Insect is a scientific calculator. It's not a computer algebra system that solves differential
    equations or computes integrals. Try *[WolframAlpha](http://www.wolframalpha.com/)* instead.
  - There is no graphical user interface with buttons for each action (*x²*, *1/x*, *DEG/RAD*,
    etc.). *[Qalculate!](http://qalculate.github.io/)* is a fantastic tool that supports both
    text- as well as graphical input.
  - Insect supports a huge range of physical units: all
    [SI units](https://en.wikipedia.org/wiki/International_System_of_Units), all units that are
    accepted by SI as well as most units of the imperial and US customary systems (and many more).
    However, if you need something even more comprehensive, try
    *[GNU units](https://www.gnu.org/software/units/)*.
  - Insect is not a general-purpose programming language. You could try
    *[Frink](https://frinklang.org/)*.
  - Insect does not have a special mode for hexadecimal or binary numbers (yet).
