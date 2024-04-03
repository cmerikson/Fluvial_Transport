# Fluvial Transport

This app calculates the transport distance along a stream or other route.
It is designed for tracking RFID tracers across multiple survey dates.

It works by assigning a location along the stream centerline for each tracer on each date. Then the distance between locations is calculated for each tracer between all dates. The scrpit relies on the `sf` and `sfnetworks` for spatial data hadnling and `data.table` for other data processing.

A total distance traveled is determined by selecting the maximum distance moved from all date pairs. Because a tracer may not be found in all survey dates, `NA` values are returned when a tracer is missing from a particular survey.

Processing times when using many files can be very slow.

Example data from surveys on Amethyst Brook, MA is provided.

Input files should be preprocessed to have a standardized tracer identification column, date column, and coordinate columns. See the example data for examples.
