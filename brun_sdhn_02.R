#' Sistem Kejadian Diskret dengan dua tipe entiti.
#'
#' Sebuah pabrik memiliki sebuah mesin pemroses yang digunakan untuk memproses dua jenis
#' pekerjaan (“job”), diberi nama jobA dan jobB. JobA datang setiap 8 menit sekali, sedangkan
#' jobB datang setiap setiap 12 menit sekali. Untuk setiap siklus produksi ada 100 jobA yang harus
#' diselesaikan, dan ada 50 jobB yang harus diselesaikan. Lama waktu yang dibutuhkan untuk
#' memproses jobA adalah menyebar Eksponensial dengan rate 1/7 per menit, sedangkan jobB
#' membutuhkan waktu yang juga menyebar eksponensial tetapi dengan rate 1/12 per menit.
#'
#' @param LamaA berbentuk fungsi yang memberikan nilai lamanya proses entiti A.
#' @param LamaB berbentuk fungsi yang memberikan nilai lamanya proses entiti B.
#' @param wkA vektor numerik terurut naik yang memberikan nilai waktu kedatanagan entiti A.
#' @param wkB vektor numerik terurut naik yang memberikan nilai waktu kedatanagan entiti B.
#' @param rsc numerik yang memberikan kapasitas sumberdaya yang tersedia.
#' @return dataset arrivals dan attributes
#' @examples
#' brun_sdhn_02(LamaA = function() rexp(1, 1/7), LamaB = function() rexp(1, 1/12),
#'              wkA = cumsum(rep(8, 100)), wkB = cumsum(rep(12, 50)))

  ######################################################
  # Program : brun_sdhn_02                             #
  # Tujuan  : Sistem antrian Sederhana dengan lebih    #
  #           dari satu jenis entiti                   #
  #           tunggal (antrian tunggal)                #
  # Tanggal : 20 Juli 2022                             #
  # Oleh    : I G.A. Anom Yudistira                    #
  # Versi : v01                                        #
  ######################################################

  # rate: laju kedatangan entiti; awal: waktu kedatangan paling awal;
  # times: pengulangan; attributes: bernilai TRUE jika data attributes
  # yang ingin ditampilkan

  # require(simmer)
  # require(tidyverse)
  # brun_sdhn_02(LamaA = function() rexp(1, 1/7), LamaB = function() rexp(1, 1/12),
  #              wkA = cumsum(rep(8, 100)), wkB = cumsum(rep(12, 50)))

brun_sdhn_02 <- function(LamaA, LamaB, wkA, wkB, rsc = 1 )
  {
  if (!is.function(LamaA) || !is.function(LamaB))
    {
      stop("argumen LamaA dan LamaB harus merupakan fungsi")
    }

  env <- simmer("sederhana2")

  traj <- trajectory() %>%
    seize("Mesin",1) %>%
    timeout_from_attribute("lama") %>%
    release("Mesin",1)

  trajA <- trajectory() %>%
    set_attribute("lama", LamaA) %>%
    join(traj)

  trajB <- trajectory() %>%
    set_attribute("lama", LamaB) %>%
    join(traj)

  env %>%
    add_resource("Mesin", 1) %>%
    add_generator("jobA", trajA, at(wkA), mon=2) %>%
    add_generator("jobB", trajB, at(wkB), mon=2) %>%
    run()

    out1 <- as_tibble(get_mon_attributes(env))
    out2 <- as_tibble(get_mon_arrivals(env))
    out <- list(Arrivals = out2, Attributes = out1)

  return(out)
  }


