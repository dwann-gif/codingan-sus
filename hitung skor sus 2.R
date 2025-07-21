data_sus <- read_excel("Skor sus 2.xlsx")

# Buat fungsi menghitung SUS
hitung_sus <- function(jawaban) {
  total <- 0
  for (i in 1:10) {
    if (i %% 2 == 1) {
      total <- total + (jawaban[i] - 1)
    } else {
      total <- total + (5 - jawaban[i])
    }
  }
  return(total * 2.5)
}

# Hitung skor SUS untuk setiap responden
skor_semua <- c()

for (i in 1:nrow(data_sus)) {
  # Ambil hanya kolom Q1–Q10
  jawaban <- as.numeric(data_sus[i, 2:11])
  
  if (any(is.na(jawaban))) {
    cat("Responden", data_sus$Nama[i], "-> Skor SUS: NA (data tidak lengkap)\n")
    skor_semua <- c(skor_semua, NA)
  } else {
    skor <- hitung_sus(jawaban)
    skor_semua <- c(skor_semua, skor)
    cat("Responden", data_sus$Nama[i], "-> Skor SUS:", skor, "\n")
  }
}

# Hitung dan tampilkan rata-rata
rata_rata <- mean(skor_semua, na.rm = TRUE)
cat("Rata-rata Skor SUS:", rata_rata, "\n")