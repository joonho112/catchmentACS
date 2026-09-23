# T-S3-04 print() emits a cli_h1 'catchmentACS run result' header

    Code
      header_only <- attr(out, "cacs_run_result_metadata")
      cat("== print.cacs_run_result structural snapshot ==\n")
    Output
      == print.cacs_run_result structural snapshot ==
    Code
      cat(sprintf("provider           : %s\n", header_only$provider))
    Output
      provider           : osrm
    Code
      cat(sprintf("profile            : %s\n", header_only$profile))
    Output
      profile            : car
    Code
      cat(sprintf("drive_times (n)    : %d\n", length(header_only$drive_times)))
    Output
      drive_times (n)    : 3
    Code
      cat(sprintf("n_sites_total      : %d\n", header_only$n_sites_total))
    Output
      n_sites_total      : 3
    Code
      cat(sprintf("n_sites_success    : %d\n", header_only$n_sites_success))
    Output
      n_sites_success    : 3
    Code
      cat(sprintf("n_sites_failed     : %d\n", header_only$n_sites_failed))
    Output
      n_sites_failed     : 0
    Code
      cat(sprintf("skipped_geoids (n) : %d\n", length(header_only$skipped_geoids)))
    Output
      skipped_geoids (n) : 0
    Code
      cat(sprintf("metadata keys      : %s\n", paste(sort(names(header_only)),
      collapse = ",")))
    Output
      metadata keys      : cacs_version,drive_times,generated_at,moe_fallback_summary,n_sites_failed,n_sites_success,n_sites_total,profile,provider,skipped_geoids,wall_clock_seconds

# T-S3-06 print(summary()) emits structured cli sections

    Code
      cat("== print.cacs_run_summary structural snapshot ==\n")
    Output
      == print.cacs_run_summary structural snapshot ==
    Code
      cat(sprintf("n_rows            : %d\n", s$n_rows))
    Output
      n_rows            : 171
    Code
      cat(sprintf("n_sites           : %d\n", s$n_sites))
    Output
      n_sites           : 3
    Code
      cat(sprintf("n_variables       : %d\n", s$n_variables))
    Output
      n_variables       : 19
    Code
      cat(sprintf("n_drive_times     : %d\n", s$n_drive_times))
    Output
      n_drive_times     : 3
    Code
      cat(sprintf("rates_breakdown n : %d\n", nrow(s$rates_breakdown)))
    Output
      rates_breakdown n : 5
    Code
      cat(sprintf("n_tracts fivenum n: %d\n", length(s$n_tracts_summary)))
    Output
      n_tracts fivenum n: 5
    Code
      cat(sprintf("moe_fallback NA?  : %s\n", if (is.na(s$moe_fallback_rate)
      ) "yes" else "no"))
    Output
      moe_fallback NA?  : no

