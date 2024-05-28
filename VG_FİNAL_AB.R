# Gerekli kütüphaneleri yükleme

install.packages("ggplot2")
install.packages("dplyr") # pipe operatörünü kullanmak için
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("ggcorrplot")


# Yüklenen Kütüphaneleri Çağırma

library(ggplot2)
library(dplyr)
library(reshape2)

# Veriyi kontrol et
print(head(nomofobi))

# Cinsiyet değişkeninin dağılımını hesaplama
cinsiyet_sayim <- nomofobi %>%
  count(cinsiyet) %>%
  mutate(pct = n / sum(n) * 100)

# Pasta grafiği oluşturma
ggplot(cinsiyet_sayim,aes(x = "", y = pct, fill = cinsiyet)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5))+
  labs(title = "Cinsiyet Dağılımı") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# Bölüm değişkeninin dağılımını hesaplama
bolum_sayim <- nomofobi %>%
  count(Bolum) %>%
  mutate(pct = n / sum(n) * 100)

# Pasta grafiği oluşturma
ggplot(bolum_sayim,aes(x = "", y = pct, fill = Bolum)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5))+
  labs(title = "Cinsiyet Dağılımı") +
  labs(title = "Bölüm Dağılımı") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())



# Veri setinizi yükleme
nomofobi <- read.csv("nomofobi.csv")

# Veri çerçevesini uzun formata dönüştürme
data_long <- reshape2::melt(nomofobi, id.vars = "cinsiyet", 
                            measure.vars = c("Depresyon", "Anksiyete", "Stres", "Yalnizlik", "Yaşam_Doyumu", 
                                             "FoMO", "NOMOFOBİ_TOPLAM"),
                            variable.name = "Degisken", 
                            value.name = "Deger")


# ggplot2 ile kutu grafikleri oluşturma (Cinsiyete Göre Psiko-Demografik Özelliklerin Dağılımı)
ggplot(data_long, aes(x = cinsiyet, y = Deger, fill = cinsiyet)) +
  geom_boxplot(outliers = FALSE, staplewidth = 0.5, alpha=0.9) +
  facet_wrap(~ Degisken, scales = "free_y") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  labs(title = "Cinsiyete Göre Psiko-Demografik Özelliklerin Dağılımı",
       x = "Cinsiyet",
       y = "Değer") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette="Set3")



# Korelasyon Matrisi görselleştirme

# Korelasyon hesaplamak istediğiniz değişkenlerin listesini oluşturma
variables <- c("Depresyon", "Anksiyete", "Stres", "Yalnizlik", "Yaşam_Doyumu", "FoMO", "NOMOFOBİ_TOPLAM")

# Korelasyon matrisini hesaplama
cor_matrix <- nomofobi %>%
  select(all_of(variables)) %>%
  cor(use = "complete.obs")

# Korelasyon matrisini uzun formata dönüştürme
cor_matrix_melt <- melt(cor_matrix)

# Korelogram oluşturma
ggplot(cor_matrix_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 1.5)), size = 2) + # Korelasyon katsayılarının yazı boyutu
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Korelasyon") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 30, vjust = 1, 
                                   size = 8, hjust = 1), # X ekseni yazı boyutu
        axis.text.y = element_text(size = 8), # Y ekseni yazı boyutu
        plot.title = element_text(size = 12)) + # Başlık yazı boyutu
  ggtitle("Korelasyon Matrisi") +
  coord_fixed()


# Akıllı Telefon Kullanım Yılına göre Nomofobi Düzeyinin Bölüme Göre Dağılımı

# ggplot ile kutu grafiği oluşturma
ggplot(nomofobi, aes(x = Akilli_Telefon_Kullanım_Yili, y = NOMOFOBİ_TOPLAM, fill = Bolum)) +
  geom_boxplot(outliers = FALSE, staplewidth = 0.5, alpha=0.9) +
  labs(x = "Akıllı Telefon Kullanım Yılı", y = "Nomofobi Düzeyi", title = "Bölüme ve Akıllı Telefon Yılına Göre Nomofobi Düzeyi") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, 
                                   size = 8, hjust = 1), # X ekseni yazı boyutu
        axis.text.y = element_text(size = 8), # Y ekseni yazı boyutu
        plot.title = element_text(size = 12))+ # Başlık yazı boyutu
        scale_fill_brewer(palette="Set3")


# Günlük Akıllı Telefon Kullanım Saatine göre Nomofobi Düzeyinin Bölüme Göre Dağılımı

# ggplot ile kutu grafiği oluşturma
ggplot(nomofobi, aes(x = Gunluk_Akilli_Telefon_Sure, y = NOMOFOBİ_TOPLAM, fill = Bolum)) +
  geom_boxplot(outliers = FALSE, staplewidth = 0.5, alpha=0.9) +
  labs(x = "Günlük Akıllı Telefon Kullanım saati", y = "Nomofobi Düzeyi", title = "Bölüme ve Günlük Akıllı Telefon Süresine Göre Nomofobi Düzeyi") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, 
                                   size = 8, hjust = 1), # X ekseni yazı boyutu
        axis.text.y = element_text(size = 8), # Y ekseni yazı boyutu
        plot.title = element_text(size = 12))+ # Başlık yazı boyutu
        scale_fill_brewer(palette="Set2")


# Bölüme Göre Akıllı Telefon Kullanım Yılının Dağılımı (GENOM BAR)

# ggplot ile çubuk grafiği oluşturma
ggplot(nomofobi, aes(x = Akilli_Telefon_Kullanım_Yili, y = Bolum, fill = Bolum)) +
  geom_bar(stat = "identity") +
  labs(x = "Akıllı Telefon Kullanım Yılı", y = "Bölüm", title = "Bölüme Göre Akıllı Telefon Kullanma Yılı") +
  theme_minimal()



# Akıllı Telefon Kullanım Yılına Göre Nomofobi Düzeyinin Bölüme Göre Dağılımı (ISI HARİTASI DENEME)

# Isı haritası oluşturma
ggplot(nomofobi, aes(x = Akilli_Telefon_Kullanım_Yili, y = reorder(Bolum, NOMOFOBİ_TOPLAM), fill = NOMOFOBİ_TOPLAM)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray") + # Eksik değerleri gray (gri) ile göster) 
  labs(x = "Akıllı Telefon Kullanım Yılı", y = "Bölüm", fill = "Nomofobi Düzeyi") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 8))



