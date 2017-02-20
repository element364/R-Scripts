library(phonTools)

sound <- loadsound('c:/Users/User/Dropbox/Programming/it open/10/task.wav')
par(mfrow = c(2, 1))
plot(sound)
powertrack(sound)
#findformants(sound)
#filtered1 = Ffilter (sound, ffs = c(4000,7000), bw = 200, verify = FALSE)
#spectrogram (filtered1, maxfreq = 11025)