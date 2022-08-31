library(dplyr)
library(foreign)
library(nnet)
library(ordinal)
library(MASS)
library(pscl)
library(lmtest)
library(zoo)
library(aod)
library(readxl)
library(tidyverse)

#buka file
setwd("~/backup/document/all manuskrip/lk2pk")
data_clean_lk2pk <- read_excel("data_clean_lk2pk.xlsx")
str(data_clean_lk2pk)
head(data_clean_lk2pk)

#menghitung usia
data_clean_lk2pk$usia = as.numeric(difftime(Sys.Date(),data_clean_lk2pk$`Tanggal lahir bapak/ibu`, units = "weeks"))/52.25
data_clean_lk2pk$usia <- round(data_clean_lk2pk$usia, 0)
data_clean_lk2pk$usia_balita = as.numeric(difftime(Sys.Date(),data_clean_lk2pk$`Tanggal lahir balita pertama`, units = "weeks"))/52.25
data_clean_lk2pk$usia_balita <- round(data_clean_lk2pk$usia_balita, 1)


#mengubah kolname perkawinan, pendidikan, tabungan, sumber_air, penghasilan, status_gizi, usia, usia_balita, PB, BB, pengurangan_penghasilan, gender_balita
colnames(data_clean_lk2pk)[which(names(data_clean_lk2pk) == "Status perkawinan")] <- "perkawinan"
colnames(data_clean_lk2pk)[which(names(data_clean_lk2pk) == "Status pendidikan tertinggi yang ditamatkan bapak/ibu?")] <- "pendidikan"
colnames(data_clean_lk2pk)[which(names(data_clean_lk2pk) == "Status pendidikan tertinggi yang ditamatkan bapak/ibu?")] <- "pendidikan"
#data_clean_lk2pk$status_gizi <- data_clean_lk2pk$`Status Gizi 
#+ (BB menurut PB/TB)`
colnames(data_clean_lk2pk)[which(names(data_clean_lk2pk) == "Apakah bapak/ibu memiliki tabungan")] <- "tabungan"
colnames(data_clean_lk2pk)[which(names(data_clean_lk2pk) == "Sumber air yang paling sering digunakan untuk minum")] <- "sumber_air"
data_clean_lk2pk$penghasilan <- data_clean_lk2pk$`Penghasilan perbulan (jumlah penghasilan bapak dan ibu)`
colnames(data_clean_lk2pk)[which(names(data_clean_lk2pk) == "BB pertama")] <- "BB"
colnames(data_clean_lk2pk)[which(names(data_clean_lk2pk) == "PB pertama")] <- "PB"
data_clean_lk2pk$pengurangan_penghasilan <- data_clean_lk2pk$`Perbedaan Pengahsilan Sebelum dan Sesudah Pandemi`
data_clean_lk2pk$gender_balita <- data_clean_lk2pk$`Jenis Kelamin 1`
data_clean_lk2pk$kat_phk <- ifelse(data_clean_lk2pk$PHK == "Ya", 1, 0)


#menambah ID
data_clean_lk2pk$ID <- seq.int(nrow(data_clean_lk2pk))

#memasukkan dalam df baru
clean_lk2pk <- data.frame(ID = data_clean_lk2pk$ID, perkawinan = data_clean_lk2pk$perkawinan, pendidikan = data_clean_lk2pk$pendidikan, 
                          pekerjaan_ibu = data_clean_lk2pk$`Pekerjaan Ibu`, pekerjaan_bapak = data_clean_lk2pk$`Pekerjaan bapak`,
                          status_gizi = data_clean_lk2pk$status_gizi,
                          usia = data_clean_lk2pk$usia, usia_balita = data_clean_lk2pk$usia_balita, PB = data_clean_lk2pk$PB, BB = data_clean_lk2pk$BB, 
                          pengurangan_penghasilan = data_clean_lk2pk$pengurangan_penghasilan, gender_balita = data_clean_lk2pk$gender_balita, penghasilan = data_clean_lk2pk$penghasilan, phk = data_clean_lk2pk$kat_phk,
                          PHK = data_clean_lk2pk$kat_phk, sumber_air = data_clean_lk2pk$sumber_air,
                          anc = data_clean_lk2pk$ANC, menyusui = data_clean_lk2pk$Menyusui, status_kemiskinan = data_clean_lk2pk$Status_Kemiskinan,
                          tabungan = data_clean_lk2pk$`Apakah bapak/ibu memilik tabungan`) 

# coba mutate karakter kawin
library(dplyr) 
nf <- data_clean_lk2pk %>% mutate(perkawinan = replace(perkawinan, match("1", ID), "kawin"))

#dummy variable

clean_lk2pk$kat_penghasilan_bulan_atas <- ifelse(clean_lk2pk$penghasilan == '> Rp. 3.200.000', 1, 0)
clean_lk2pk$kat_penghasilan_bulan_bawah <- ifelse(clean_lk2pk$penghasilan == '< Rp. 3.200.000', 1, 0)
clean_lk2pk$kat_jk_laki <- ifelse(clean_lk2pk$gender_balita == 'L', 1, 0)
clean_lk2pk$kat_jk_perempuan <- ifelse(clean_lk2pk$gender_balita == 'P', 1, 0)
library(fastDummies)
clean_lk2pk <- dummy_cols(clean_lk2pk, select_columns = 'status_gizi')
clean_lk2pk <- dummy_cols(clean_lk2pk, select_columns = 'pekerjaan_ibu')
clean_lk2pk <- dummy_cols(clean_lk2pk, select_columns = 'pekerjaan_bapak')
clean_lk2pk <- dummy_cols(clean_lk2pk, select_columns = 'pendidikan')


#clean_lk2pk = subset(clean_lk2pk, select = -c(status_gizi_Normal) )
colnames(clean_lk2pk)[which(names(clean_lk2pk) == "status")] <- "perkawinan"
clean_lk2pk$`status_gizi_Gizi kurang` <- factor(clean_lk2pk$`status_gizi_Gizi kurang`)
colnames(clean_lk2pk)[which(names(clean_lk2pk) == "status_gizi_Gizi kurang")] <- "kurang"
colnames(clean_lk2pk)[which(names(clean_lk2pk) == "status_gizi_Berisiko gizi lebih")] <- "berisiko_lebih"
colnames(clean_lk2pk)[which(names(clean_lk2pk) == "status_gizi_Gizi lebih")] <- "lebih"
colnames(clean_lk2pk)[which(names(clean_lk2pk) == "status_gizi_Normal")] <- "normal"
colnames(clean_lk2pk)[which(names(clean_lk2pk) == "status_gizi_Obesitas")] <- "obesitas"
colnames(clean_lk2pk)[which(names(clean_lk2pk) == "pekerjaan_ibu_Pegawai Swasta")] <- "kerja_ibu_swasta"
colnames(clean_lk2pk)[which(names(clean_lk2pk) == "pekerjaan_ibu_PNS/TIN/Polri/BUMN/BUMD")] <- "kerja_ibu_pemerintah"
colnames(clean_lk2pk)[which(names(clean_lk2pk) == "pekerjaan_ibu_Wiraswasta")] <- "kerja_ibu_wiraswasta"
colnames(clean_lk2pk)[which(names(clean_lk2pk) == "pekerjaan_ibu_Tidak Bekerja")] <- "kerja_ibu_tidak_kerja"
clean_lk2pk$kerja_bapak_buruh <- clean_lk2pk$`pekerjaan_bapak_Buruh/Buruh pabrik/Sopir/Ojek/Pembantu`
clean_lk2pk$kerja_bapak_pemerintah <- clean_lk2pk$`pekerjaan_bapak_PNS/TIN/Polri/BUMN/BUMD`
clean_lk2pk$kerja_bapak_swasta <- clean_lk2pk$`pekerjaan_bapak_Pegawai Swasta`
clean_lk2pk$kerja_bapak_wiraswasta <- clean_lk2pk$pekerjaan_bapak_Wiraswasta
clean_lk2pk$kerja_bapak_tidak_kerja <- clean_lk2pk$`pekerjaan_bapak_Tidak Bekerja`
clean_lk2pk$pendidikan_SD <- clean_lk2pk$`pendidikan_Tidak Tamat SD/MI`
clean_lk2pk$pendidikan_SMP <- clean_lk2pk$`pendidikan_Tamat SMP/MTs`
clean_lk2pk$pendidikan_SMA <- clean_lk2pk$`pendidikan_Tamat SMA/MAN`
clean_lk2pk$pendidikan_kuliah <- clean_lk2pk$`pendidikan_Tamat PT`
clean_lk2pk$non_phk <- ifelse(data_clean_lk2pk$PHK == "Tidak", 1, 0)
clean_lk2pk$kerja_ibu_swasta <- clean_lk2pk$`pekerjaan_ibu_Pegawai Swasta`
clean_lk2pk$kerja_ibu_pemerintah <- clean_lk2pk$`pekerjaan_ibu_PNS/TIN/Polri/BUMN/BUMD`
clean_lk2pk$kerja_ibu_wiraswasta <- clean_lk2pk$pekerjaan_ibu_Wiraswasta
clean_lk2pk$kerja_ibu_tidak_kerja <- clean_lk2pk$`pekerjaan_ibu_Tidak Bekerja`


x = ifelse(clean_lk2pk$pengurangan_penghasilan>2000000,0,1)
clean_lk2pk <- cbind(clean_lk2pk, kat_pengurangan_penghasilan = x)
clean_lk2pk$kat_anc <- ifelse(clean_lk2pk$anc == 'Lengkap', 1, 0)
clean_lk2pk$kat_non_anc <- ifelse(clean_lk2pk$anc == 'Non-lengkap', 1, 0)

clean_lk2pk$kat_menyusui <- ifelse(clean_lk2pk$menyusui == 'Tidak', 1, 0)
clean_lk2pk$kat_non_menyusui <- ifelse(clean_lk2pk$menyusui == 'Ya', 1, 0)

clean_lk2pk$kat_kemiskinan <- ifelse(clean_lk2pk$status_kemiskinan == 'Non-BGK', 0, 1)
clean_lk2pk$kat_non_kemiskinan <- ifelse(clean_lk2pk$status_kemiskinan == 'BGK', 0, 1)

clean_lk2pk$kat_tabungan <- ifelse(clean_lk2pk$tabungan == 'Ya', 1, 0)
clean_lk2pk$kat_non_tabungan <- ifelse(clean_lk2pk$tabungan == 'Tidak', 1, 0)

clean_lk2pk$usia_balita_kurang <- ifelse(clean_lk2pk$usia_balita < 1, 1, 0)
clean_lk2pk$usia_balita_lebih <- ifelse(clean_lk2pk$usia_balita > 0.99, 1, 0)

clean_lk2pk$kat_air_sumur <- ifelse(clean_lk2pk$sumber_air == 'Sumur', 1, 0)
clean_lk2pk$kat_air_kemasan <- ifelse(clean_lk2pk$sumber_air == 'Air kemasan', 1, 0)
clean_lk2pk$kat_air_pam <- ifelse(clean_lk2pk$sumber_air == 'Air PAM', 1, 0)




#tabulasi
table (clean_lk2pk$kurang, clean_lk2pk$PHK)
table (clean_lk2pk$kurang, clean_lk2pk$status_kemiskinan)
table (clean_lk2pk$kurang, clean_lk2pk$gender_balita)
table (clean_lk2pk$kurang, clean_lk2pk$penghasilan)
table (clean_lk2pk$kurang, clean_lk2pk$sumber_air)
table(clean_lk2pk$kurang, clean_lk2pk$pekerjaan_bapak)
table(clean_lk2pk$kurang, clean_lk2pk$pekerjaan_ibu)
table(clean_lk2pk$kurang, clean_lk2pk$pendidikan)
table (clean_lk2pk$kurang, clean_lk2pk$anc)
table (clean_lk2pk$kurang, clean_lk2pk$tabungan)
table (clean_lk2pk$kurang, clean_lk2pk$menyusui)
table(clean_lk2pk$kurang, clean_lk2pk$usia_balita)

#uji chi-square
chisq.test(clean_lk2pk$status_gizi, clean_lk2pk$sumber_air)
chisq.test(clean_lk2pk$status_gizi, clean_lk2pk$gender_balita)
chisq.test(clean_lk2pk$status_gizi, clean_lk2pk$tabungan)
chisq.test(clean_lk2pk$status_gizi, data_clean_lk2pk$PHK)
clean_lk2pk$kat_penghasilan_bulan_car <- as.character(clean_lk2pk$kat_penghasilan_bulan)
chisq.test(clean_lk2pk$status_gizi,clean_lk2pk$kat_penghasilan_bulan_car )
clean_lk2pk$kat_pengurangan_penghasilan_car <- as.character(clean_lk2pk$kat_pengurangan_penghasilan)
chisq.test(clean_lk2pk$status_gizi, clean_lk2pk$kat_pengurangan_penghasilan_car)
chisq.test(clean_lk2pk$status_gizi, clean_lk2pk$anc)
chisq.test(clean_lk2pk$status_gizi, clean_lk2pk$menyusui)
chisq.test(clean_lk2pk$status_gizi, clean_lk2pk$status_kemiskinan)


#training dan testing coba
ho <- filter(clean_lk2pk,kurang==0)
hd <- filter(clean_lk2pk,kurang==1)
set.seed(10)
acak.ho <- sample(1:nrow(ho), 0.7*nrow(ho))
acak.hd <- sample(1:nrow(hd), 0.7*nrow(hd))
kurang.tr <- rbind(ho[acak.ho,],hd[acak.hd,])
kurang.test <- rbind(ho[-acak.ho,],hd[-acak.hd,])
mylogit <- glm(kurang ~ kat_penghasilan_bulan, data = kurang.test, family = "binomial")
summary(mylogit)
prob.prediksi<-predict(mylogit, kurang.test, type="response")
prediksi<-ifelse(prob.prediksi>0.7,"Kurang","Tidak Kurang")
pred.aktual<-data.frame(Prediksi =prediksi, Aktual =kurang.test$kurang)
head(pred.aktual)
install.packages("caret")
ifelse(kurang.test$kurang == 0, "Tidak Kurang", "Kurang")
kurang.test$kurang <- as.factor(kurang.test$kurang)
library(caret)
confusionMatrix(as.factor(prediksi), kurang.test$kurang)


#regresi logistik bivariat

library('foreign')
library('nnet')
library('ordinal')
library('MASS')
library('pscl')
library('lmtest')
library('zoo')
library(aod)
str(clean_lk2pk)
mylogit1 <- glm(kurang  ~ PHK, data = clean_lk2pk, family = "binomial" )
summary(mylogit1)
confint(mylogit1, level = 0.95)
exp(cbind(OR = coef(mylogit1), confint(mylogit1, level = 0.95)))
mylogit1a <- glm(kurang  ~ non_phk, data = clean_lk2pk, family = "binomial" )
summary(mylogit1a)
exp(cbind(OR = coef(mylogit1a), confint(mylogit1a, level = 0.95)))
mylogit2 <- glm(kurang  ~ kat_jk, data = clean_lk2pk, family = "binomial" )
summary(mylogit2)
exp(cbind(OR = coef(mylogit2), confint(mylogit2)))
mylogit2a <- glm(kurang  ~ kat_jk_perempuan, data = clean_lk2pk, family = "binomial" )
summary(mylogit2a)
exp(cbind(OR = coef(mylogit2a), confint(mylogit2a)))
mylogit3 <- glm(kurang  ~ kat_anc, data = clean_lk2pk, family = "binomial" )
summary(mylogit3)
exp(cbind(OR = coef(mylogit3), confint(mylogit3)))
mylogit4 <- glm(kurang  ~ kat_tabungan, data = clean_lk2pk, family = "binomial" )
summary(mylogit4)
exp(cbind(OR = coef(mylogit4), confint(mylogit4)))
mylogit5 <- glm(kurang  ~ kat_menyusui, data = clean_lk2pk, family = "binomial" )
summary(mylogit5)
exp(cbind(OR = coef(mylogit5), confint(mylogit5)))
mylogit6 <- glm(kurang  ~ kat_penghasilan_bulan_bawah, data = clean_lk2pk, family = "binomial" )
summary(mylogit6)
exp(cbind(OR = coef(mylogit6), confint(mylogit6)))
mylogit6a <- glm(kurang  ~ kat_penghasilan_bulan_atas, data = clean_lk2pk, family = "binomial" )
summary(mylogit6a)
exp(cbind(OR = coef(mylogit6a), confint(mylogit6a)))
mylogit7 <- glm(kurang  ~ kat_kemiskinan, data = clean_lk2pk, family = "binomial" )
summary(mylogit7)
exp(cbind(OR = coef(mylogit7), confint(mylogit7)))
mylogit7a <- glm(kurang  ~ kat_non_kemiskinan, data = clean_lk2pk, family = "binomial" )
summary(mylogit7a)
exp(cbind(OR = coef(mylogit7a), confint(mylogit7a)))
mylogit8 <- glm(kurang  ~ kerja_bapak_buruh, data = clean_lk2pk, family = "binomial" )
summary(mylogit8)
exp(cbind(OR = coef(mylogit8), confint(mylogit8)))
mylogit8a <- glm(kurang  ~ kerja_bapak_pemerintah, data = clean_lk2pk, family = "binomial" )
summary(mylogit8a)
exp(cbind(OR = coef(mylogit8a), confint(mylogit8a)))
mylogit8b <- glm(kurang  ~ kerja_bapak_swasta, data = clean_lk2pk, family = "binomial" )
summary(mylogit8b)
exp(cbind(OR = coef(mylogit8b), confint(mylogit8b)))
mylogit8c <- glm(kurang  ~ kerja_bapak_tidak_kerja, data = clean_lk2pk, family = "binomial" )
summary(mylogit8c)
exp(cbind(OR = coef(mylogit8c), confint(mylogit8c)))
mylogit8d <- glm(kurang  ~ kerja_bapak_wiraswasta, data = clean_lk2pk, family = "binomial" )
summary(mylogit8d)
exp(cbind(OR = coef(mylogit8d), confint(mylogit8d)))
mylogit9 <- glm(kurang  ~ kerja_ibu_tidak_kerja , data = clean_lk2pk, family = "binomial" )
summary(mylogit9)
exp(cbind(OR = coef(mylogit9), confint(mylogit9)))
mylogit9b <- glm(kurang  ~ kerja_ibu_swasta , data = clean_lk2pk, family = "binomial" )
summary(mylogit9b)
exp(cbind(OR = coef(mylogit9b), confint(mylogit9b)))
mylogit9c <- glm(kurang  ~ kerja_ibu_pemerintah , data = clean_lk2pk, family = "binomial" )
summary(mylogit9c)
exp(cbind(OR = coef(mylogit9c), confint(mylogit9c)))
mylogit9d <- glm(kurang  ~ kerja_ibu_wiraswasta , data = clean_lk2pk, family = "binomial" )
summary(mylogit9d)
exp(cbind(OR = coef(mylogit9d), confint(mylogit9d)))
mylogit10 <- glm(kurang  ~ pendidikan_SD , data = clean_lk2pk, family = "binomial" )
summary(mylogit10)
exp(cbind(OR = coef(mylogit10), confint(mylogit10)))
mylogit10b <- glm(kurang  ~ pendidikan_SMP , data = clean_lk2pk, family = "binomial" )
summary(mylogit10b)
exp(cbind(OR = coef(mylogit10b), confint(mylogit10b)))
mylogit10c <- glm(kurang  ~ pendidikan_SMA , data = clean_lk2pk, family = "binomial" )
summary(mylogit10c)
exp(cbind(OR = coef(mylogit10c), confint(mylogit10c)))
mylogit11 <- glm(kurang  ~ kat_anc , data = clean_lk2pk, family = "binomial" )
summary(mylogit11)
exp(cbind(OR = coef(mylogit11), confint(mylogit11)))
mylogit11b <- glm(kurang  ~ kat_non_anc , data = clean_lk2pk, family = "binomial" )
summary(mylogit11b)
exp(cbind(OR = coef(mylogit11b), confint(mylogit11b)))
mylogit12 <- glm(kurang  ~ kat_tabungan , data = clean_lk2pk, family = "binomial" )
summary(mylogit12)
exp(cbind(OR = coef(mylogit12), confint(mylogit12)))
mylogit12b <- glm(kurang  ~ kat_non_tabungan , data = clean_lk2pk, family = "binomial" )
summary(mylogit12b)
exp(cbind(OR = coef(mylogit12b), confint(mylogit12b)))
mylogit13 <- glm(kurang  ~ usia_balita_kurang , data = clean_lk2pk, family = "binomial" )
summary(mylogit13)
exp(cbind(OR = coef(mylogit13), confint(mylogit13)))
mylogit13b <- glm(kurang  ~ usia_balita_lebih , data = clean_lk2pk, family = "binomial" )
summary(mylogit13b)
exp(cbind(OR = coef(mylogit13b), confint(mylogit13b)))
mylogit14 <- glm(kurang  ~  kat_air_kemasan, data = clean_lk2pk, family = "binomial" )
summary(mylogit14)
exp(cbind(OR = coef(mylogit14), confint(mylogit14)))
mylogit14b <- glm(kurang  ~  kat_air_pam, data = clean_lk2pk, family = "binomial" )
summary(mylogit14b)
exp(cbind(OR = coef(mylogit14b), confint(mylogit14b)))
mylogit14c <- glm(kurang  ~  kat_air_sumur, data = clean_lk2pk, family = "binomial" )
summary(mylogit14c)
exp(cbind(OR = coef(mylogit14c), confint(mylogit14c)))
mylogit15 <- glm(kurang  ~  kat_menyusui, data = clean_lk2pk, family = "binomial" )
summary(mylogit15)
exp(cbind(OR = coef(mylogit15), confint(mylogit15)))
mylogit15b <- glm(kurang  ~  kat_non_menyusui, data = clean_lk2pk, family = "binomial" )
summary(mylogit15b)
exp(cbind(OR = coef(mylogit15b), confint(mylogit15b)))



#multivariat
mylogit_total1 <- glm( kurang ~ PHK + pendidikan_SD + kat_non_anc, data = clean_lk2pk, family = "binomial")
summary(mylogit_total1)
confint(mylogit_total1)
exp(cbind(OR = coef(mylogit_total1), confint(mylogit_total1)))
mylogit_total1a <- glm( kurang ~ non_phk + pendidikan_SD + kat_non_anc, data = clean_lk2pk, family = "binomial")
summary(mylogit_total1a)
confint(mylogit_total1a)
exp(cbind(OR = coef(mylogit_total1a), confint(mylogit_total1a)))
mylogit_total1b <- glm( kurang ~ non_phk + pendidikan_SMP + kat_non_anc, data = clean_lk2pk, family = "binomial")
summary(mylogit_total1b)
confint(mylogit_total1b)
exp(cbind(OR = coef(mylogit_total1b), confint(mylogit_total1b)))
mylogit_total1c <- glm( kurang ~ non_phk + pendidikan_SMA + kat_non_anc, data = clean_lk2pk, family = "binomial")
summary(mylogit_total1c)
confint(mylogit_total1c)
exp(cbind(OR = coef(mylogit_total1c), confint(mylogit_total1c)))
mylogit_total1d <- glm( kurang ~ PHK + pendidikan_SD + kat_anc, data = clean_lk2pk, family = "binomial")
summary(mylogit_total1d)
confint(mylogit_total1d)
exp(cbind(OR = coef(mylogit_total1d), confint(mylogit_total1d)))
mylogit_total2 <- glm( kurang ~ kat_kemiskinan + kerja_bapak_tidak_kerja + kat_non_anc , data = clean_lk2pk, family = "binomial")
summary(mylogit_total2)
confint(mylogit_total2)
exp(cbind(OR = coef(mylogit_total2), confint(mylogit_total2)))
mylogit_total2b <- glm( kurang ~ kat_non_kemiskinan + kerja_bapak_tidak_kerja + kat_non_anc , data = clean_lk2pk, family = "binomial")
summary(mylogit_total2b)
confint(mylogit_total2b)
exp(cbind(OR = coef(mylogit_total2b), confint(mylogit_total2b)))
mylogit_total2c <- glm( kurang ~ kat_kemiskinan + kerja_bapak_tidak_kerja + kat_anc , data = clean_lk2pk, family = "binomial")
summary(mylogit_total2c)
confint(mylogit_total2c)
exp(cbind(OR = coef(mylogit_total2c), confint(mylogit_total2c)))
mylogit_total2d <- glm( kurang ~ kat_kemiskinan + kerja_bapak_wiraswasta + kat_non_anc , data = clean_lk2pk, family = "binomial")
summary(mylogit_total2d)
confint(mylogit_total2d)
exp(cbind(OR = coef(mylogit_total2d), confint(mylogit_total2d)))


#uji model
install.packages('ResourceSelection')
library(ResourceSelection)
pR2(mylogit_total1)
pR2(mylogit_total2)
hl <- hoslem.test(mylogit_total1$y, fitted(mylogit_total1), g=10)
hl$p.value
hl2 <- hoslem.test(mylogit_total2$y, fitted(mylogit_total2), g=10)
hl2$p.value