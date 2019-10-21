FAQ
---

  - Why are Celsius and Fahrenheit not supported?

    Compared to the SI unit [Kelvin](https://en.wikipedia.org/wiki/Kelvin) and in contrast to all
    other units, Celsius and Fahrenheit require an additive offset when converting into and from
    other temperature units. This additive offset leads to all kinds of ambiguities when performing
    calculations in these units. Adding two temperatures in Celsius, for example, is only
    meaningful if one of them is seen as an offset value (rather than an absolute temperature).
    Insect is primarily a scientific calculator (as opposed to a unit conversion tool) and
    therefore focuses on getting physical calculations right.

    Even though *°C* and *°F* are not supported as built-in units, there are helper functions to
    convert to and from Celsius (and Fahrenheit):

      - `fromCelsius` takes a **scalar value** that represents a temperature in Celsius and returns
        a corresponding **temperature in Kelvin**:

        ```
        > fromCelsius(0)

           = 273.15 K

        > k_B * fromCelsius(23) to meV

           = 25.5202 meV
        ```

      - `toCelsius` takes a **temperature in Kelvin** and returns a **scalar value** that
        represents the corresponding temperature in Celsius:

        ```
        > toCelsius(70 K)

           = -203.15

        > toCelsius(25 meV / k_B)

           = 16.963
        ```

  - Why is `1/2 x` parsed as `1/(2x)`?

    *Implicit* multiplication (without an explicit multiplication sign) has a higher precedence
    than division (see [operator precedence rules](#reference)). This is by design, in order to
    parse inputs like `50 cm / 2 m` as `(50 cm) / (2 m)`. If you meant *½ · x*, write `1/2 * x`.

  - What is the internal numerical precision?

    By default, Insect shows 6 significant digits in the result of the calculation. However,
    the internal numerical precision is much higher (30 digits).

  - How does the conversion operator work?

    The conversion operator `->` attempts to convert the physical quantity on its left hand side
    to the *unit of the expression* on its right hand side. This means that you can write an
    arbitrary expression on the right hand side (but only the unit part will be extracted). For
    example:

    ```
    # simple unit conversion:
    > 120 km/h -> mph

      = 74.5645 mi/h

    # expression on the right hand side:
    > 120 m^3 -> km * m^2

      = 0.12 m²·km

    # convert x1 to the same unit as x2:
    > x1 = 50 km / h
    > x2 = 3 m/s -> x1

      x2 = 10.8 km/h
    ```

  - What is the relation between the units `RPM`, `rad/s`, `deg/s` and `Hz`?

    The unit [`RPM`](https://en.wikipedia.org/wiki/Revolutions_per_minute) (revolutions per
    minute) is defined via `1 RPM = 1 / minute` where the `1` on the right hand side symbolizes
    "1 revolution".

    As the base unit is the same (`1 / second`), `RPM` can be converted to `rad / s`, `deg / s` or
    `Hz`. Note, however, that `1 RPM` does *not* equal `2π rad / min` or `360° / min` or `1 Hz`, as
    some might expect. If you interested in computing the traversed angle of something that rotates
    with a given number of revolutions per minute, you need to multiply by `2π rad` or `360 °`
    because:
    ```
    1 RPM · (360°/revolution) = (1 revolution / minute) · (360° / revolution) = 360° / minute
    ```
