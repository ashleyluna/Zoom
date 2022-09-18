Zoom includes **3** easy to use transformations: *In Zoom*, *Slide*, and *Out Zoom*.

In Zoom takes an area (ContentMask) from some footage, enlarges it, and can focus on some point inside that area without affecting content outside the area.

Slide takes a slightly larger area (ContentBorderMask), that includes some border around the area used in the In Zoom transformation, and moves that area from outside the canvas, into the canvas vertically and/or horizontally.

Out Zoom takes that same area and expands until its height or width (whichever is longest) fits some larger defined area (TotalBorderMask).

Each transformation in Zoom has a "start", "end", and "duration" (defined in milliseconds). "start" and "end" determine the amount of time each animation is delayed by relative to the start and end times of the associated clip while "duration" defines the amount of time it takes to complete a transformation. Each transformation occurs at the beginning and undoes itself at the end.

This effect uses some complex math so it is maintained as a Haskell file that renders text that is copy pasted into Expression text boxes in Davinci Resolve.
