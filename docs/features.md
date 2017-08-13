Documentation
-------------

  - **Evaluate mathematical expressions**:

    ```
    1920/16*9
    2^32
    sqrt(1.4^2 + 1.5^2) * cos(pi/3)^2
    ```

    * **Operators**: addition (`+`), subtraction (`-`),
      multiplication (`*`, `·`, `×`), division (`/`, `÷`, `per`),
      exponentiation (`^`, `**`). Full list: see [*Reference*](#reference) below.

    * **Mathematical functions**: `acos`, `acosh`, `asin`, `asinh`, `atan`, `atan2`,
      `atanh`, `ceil`, `cos`, `cosh`, `exp`, `floor`, `fromCelsius`,
      `fromFahrenheit`, `gamma`, `ln`, `log`, `log10`, `round`, `sin`, `sinh`,
      `sqrt`, `tan`, `tanh`, `toCelsius`, `toFahrenheit`.

    * **High-precision numeric type** with *30* significant digits that can handle
      *very* large (or small) exponents like *10^(10^10)*.

    * **Exponential notation**: `6.022e23`.

  - **Physical units**: parsing and handling, including metric prefixes:

    ```
    2 min + 30 s
    40 kg * 9.8 m/s^2 * 150 cm
    sin(30°)
    ```

      * **Supported units**: see [*Reference*](#reference) section below.

      * **Implicit conversions**: `15 km/h * 30 min` evaluates to `7.5 km`.

      * **Useful error messages**:

        ```
        > 2 watts + 4 newton meter

        Unification error:
          Cannot convert unit N·m (base units: kg·m²·s⁻²)
                      to unit W (base units: kg·m²·s⁻³)
        ```

  - **Explicit unit conversions**: the `->` conversion operator (aliases: `→`, `➞`, `to`):

    ```
    60 mph -> m/s
    500 km/day -> km/h
    1 mrad -> degree
    52 weeks -> days
    5 in + 2 ft -> cm
    atan(30 cm / 2 m) -> degree
    6 Mbit/s * 1.5 h -> GB
    ```

  - **Variable assignments**:

    Example: mass of the earth
    ```
    r = 6000km
    vol = 4/3 * pi * r^3
    density = 5 g/cm^3
    vol * density -> kg
    ```

    Example: oscillation period of a pendulum
    ```
    len = 20 cm
    2pi*sqrt(len/g0) -> ms
    ```

      * **Predefined constants** (type `list` to see them all): speed of light (`c`),
        Planck's constant (`h_bar`), electron mass (`electronMass`), elementary charge
        (`elementaryCharge`), magnetic constant (`µ0`), electric constant (`eps0`),
        Bohr magneton (`µ_B`), Avogadro's constant (`N_A`), Boltzmann constant
        (`k_B`), gravitational acceleration (`g0`), ...

      * **Last result**: you can use `ans` (answer) to refer to the result of the
        last calculation.

  - **Unicode support**:

    ```
    λ = 2 × 300 µm
    ν = c/λ → GHz
    ```

  - **And more**: tab completion, command history (arrow keys, `Ctrl`+`R`), pretty printing, syntax
    highlighting, ...
