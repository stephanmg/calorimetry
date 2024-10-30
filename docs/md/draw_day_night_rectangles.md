# draw_day_night_rectangles

## Description

This function inks the day with a light color and night with a darker color

## Usage

```r
draw_day_night_rectangles(
  df,
  p,
  light_start = 7,
  light_end = 19,
  light_offset = 0,
  day_color = "yellow",
  night_color = "grey",
  light_cycle = c("Day", "Night")
)
```

## Arguments

* `df`: data frame
* `p`: plot object
* `light_start`: start of light cycle
* `light_end`: end of light cycle
* `light_offset`: offset of light cycle from start of day (for zeitgeber time always 0)
* `day_color`: color for light phase (day)
* `night_color`: color for dark phase (night)
* `light_cycle`: choose Day or Night

