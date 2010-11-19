Format universal times with a format-like control string. Example:

  (format-time nil "~{Dayname}, ~{Monthname} ~:@{Date}") =>
     "Monday, May 24th"
