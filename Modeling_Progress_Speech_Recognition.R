library(rstatix)
library(readr)
library(dplyr)
library(ggplot2)

url="https://raw.githubusercontent.com/hamlel/Carreras-con-Impacto/main/Speech%20Recognition%20Models%20-%20SpeechRecognition_Filter.csv"


speech<-read_csv(url(url))

speech<- speech %>% filter(Benchmark %in% c("AISHELL-1","Common Voice French",
                                            "Common Voice German","Common Voice Spanish",
                                            "LRS2","LibriSpeech test-clean",
                                            "LibriSpeech test-other","WSJ eval92")) 

 speech%>%
  group_by(Benchmark) %>%
  get_summary_stats(WER, type = "common")

 write.csv(speech%>%
   group_by(Arquitectura) %>%
   get_summary_stats(WER, type = "common"),file="arquitectura.csv")

 
 ggplot(speech,aes(reorder(Benchmark,-WER),WER/100,fill=Benchmark))+
  geom_boxplot()+coord_flip()+guides(fill=FALSE)+
   labs(x="Benchmark",y="Word Error Rate (WER %)")+
   scale_y_continuous(labels = scales::percent)
 
 ggplot(speech%>%filter(Arquitectura != "NA"),aes(reorder(Arquitectura,-WER),WER/100,fill=Arquitectura))+
   geom_boxplot()+coord_flip()+guides(fill=FALSE)+
   labs(x="Neural Network Architecture",y="Word Error Rate (WER %)")+
   scale_y_continuous(labels = scales::percent)

 
 ggplot(speech%>%filter(Benchmark %in% c("AISHELL-1","LibriSpeech test-clean",
                                         "LibriSpeech test-other","WSJ eval92")),aes(Fecha,WER/100,fill=Benchmark,shape=Benchmark))+
   geom_smooth(method=lm)+geom_point()+
   facet_wrap(~Benchmark)+guides(fill=FALSE,shape=FALSE)+
   scale_y_continuous(labels = scales::percent)+
   labs(x="Date", y="Word Error Rate (WER %)")
 
  
 ggplot(speech%>%filter(Benchmark %in% c("Common Voice French","Common Voice Spanish",
                                         "Common Voice German", "LRS2")),aes(Fecha,WER/100,fill=Benchmark,shape=Benchmark))+
   geom_smooth(method=lm)+geom_point()+
   facet_wrap(~Benchmark)+guides(fill=FALSE,shape=FALSE)+
   scale_y_continuous(labels = scales::percent)+
   labs(x="Date", y="Word Error Rate (WER %)")




