# 0.0.6 (2026-07-27)

* Use a red-black tree for the reassembly queue instead of a list to avoid
  computational complexity (OSEC-2026-11, reported by Thomas Gazagnaire,
  fixed in 08522428c957d796a6455d5aad434701c879fd4e)
* Rope fix tests (@hannesm https://git.robur.coop/robur/utcp/pulls/6)
* Rope coalesce in append (@hannesm https://git.robur.coop/robur/utcp/pulls/5,
  fixes https://github.com/robur-coop/utcp/issues/71)
* Cleanup resources in teardown (@dinosaure
  https://github.com/robur-coop/utcp/pull/73,
  @hannesm https://git.robur.coop/robur/utcp/pulls/2)
* Add num_connections (@reynir https://git.robur.coop/robur/utcp/pulls/4,
  fixes https://git.robur.coop/robur/utcp/issues/3)
* Migrate repository to https://git.robur.coop (repository is mirrored
  automatically to GitHub, issues can be reported on GitHub)
* Check bounds for buffer in User.send and User.force_enqueue
  (@hannesm https://git.robur.coop/robur/utcp/pulls/1)
* Only use ports 1024..65535 as source port
  (https://github.com/robur-coop/utcp/pull/76 @reynir @hannesm, fixes
  https://github.com/robur-coop/utcp/issues/74)
* Acknowledge a received segment even if it is not acceptable (see RFC 9293
  Section 3.10.7.4) (https://github.com/robur-coop/utcp/pull/75 @dinosaure)
* Fix how the persist timer is started (was correct in the formal model, a typo
  in the implementation) (https://github.com/robur-coop/utcp/pull/77 @dinosaure)
* Remove ipaddr-cstruct from dependency cone
  (https://github.com/robur-coop/utcp/pull/72 @dinosaure)

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
