insect
======

A fast, repl-style scientific calculator for the web.

![insect](media/insect-32x32.png)

Features
--------
- Evaluation of mathematical expressions:
  ```
  1920/16*9
  2^32
  pi*(1.4+2)²
  ```

- Parsing and handling of physical units:
  ```
  4/3 pi (6000km)³
  2min + 30s
  ```

- Explicit unit conversions
  ```
  60mph -> m/s
  500km/day -> km/h
  1mrad -> °
  52weeks -> days
  5in -> cm
  ```

- Variable assigments:
  ```
  r = 6000km
  vol = 4/3 pi r³
  density = 5g/cm³
  vol * density -> kg
  ```

- Commands:
  ```
  help, ?
  list, ls
  reset
  ```

Build
-----
```sh
pulp -w build --skip-entry-point -m Insect -O -t insect.js
```
