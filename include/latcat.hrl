-record(count, {key, val}).
-record(note, {id, timestamp=httpd_util:rfc1123_date(), lat, lon, text}).
