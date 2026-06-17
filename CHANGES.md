# 0.0.5 (2026-06-17)

* Avoid assert false, also cleanup log messages (#70 @hannesm)

# 0.0.4 (2026-05-23)

* Deuplicate the metrics sources, there's no need for each stack constructing
  their own value. The "stack-id" is present as tag in each measurement.
  (#69 @hannesm)

# 0.0.3 (2026-04-30)

* Fix sequence number arithmetics, stick to unsigned int32 with "serial number
  arithmetics" (RFC 1982) for comparison (#68 @dinosaure)

# 0.0.2 (2026-02-17)

* Fix Segment.decode when the data_offset field is too small (< 5)
  reported by @kit-ty-kate fixed by @dinosaure and @hannesm #67 (also #66)

# 0.0.1 (2026-02-09)

* Initial public release
