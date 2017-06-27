install.packages('qdap')
install.packages('XML')
install.packages('tm')
install.packages('splitstackshape')
install.packages('kernlab')
install.packages('e1071')
library(e1071)
library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(kernlab)
getwd()

GenerateVocabulary <- function(path, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
	setwd(path)
	
	files = list.files(pattern="*.xml")
	
	corpus.raw <- NULL
	i <- 0
	for (file in files) {
    # El sexo y variedad correspondan, añadir aqui
	  #author <- gsub(".xml", "", file)
	  #variety <- truth[truth$author==author,"variety"]
	  #gender <- truth[truth$author==author,"gender"]
    
    # si se cumple el sexo y variedad entonces ejecutar las cuatro lineas de abajo
    
    #if (variety = "venezuela")
    #{
		  xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
      
		  corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
      #print(corpus.raw)
		  i <- i + 1
		  if (verbose) print(paste(i, " ", file))	
    #}
	}

	corpus.preprocessed <- corpus.raw
	
	
	if (lowcase) {
		if (verbose) print("Tolower...")
		corpus.preprocessed <- tolower(corpus.preprocessed)
	}	
	
	if (punctuations) {
		if (verbose) print("Removing punctuations...")		
		corpus.preprocessed <- removePunctuation(corpus.preprocessed)
	}

	if (numbers) {
		if (verbose) print("Removing numbers...")
		corpus.preprocessed <- removeNumbers(corpus.preprocessed)
	}
  
  #swlang = 
	if (swlang!="")	{
		if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
		corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
	}

	if (swlist!="") {
		if (verbose) print("Removing provided stopwords...")
    #Elimina palabras que no queremos swlist = "es"
		corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
	}
  
  corpus.preprocessed <- removeWords(corpus.preprocessed, c("si","vía","ç","a","b","c","d","e","f","g","h","i","j","k","l","m","n","ñ","o","p","q","r","s","t","u","v","w","x","y","z","ante", "con", "contra", "de", "desde", "en", "entre", "cabe", "bajo", "hacia","hasta","para","por","según","sin","segun","so", "sobre","tras","durante","mediante","tras"))
	
	if (whitespaces) {
	  if (verbose) print("Stripping whitestpaces...")
	  corpus.preprocessed <- stripWhitespace(corpus.preprocessed) #hacerlo despues del stopwords.
	}
  
  if (verbose) print("Generating frequency terms")
	
	corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
	if (verbose) plot(corpus.frequentterms)
	
  print('--------------------------------------------------------------------------------------------------------------')

  i = 0
  for (word in corpus.frequentterms[0]){
    # word = substrRight(word, 0)
    word =  substr(word, nchar(word)-1, nchar(word))
    corpus.frequentterms[0][i] = word
    i = i + 1
  }
  #print(corpus.frequentterms)
  
	return (corpus.frequentterms)
}

GenerateBoW <- function(path, vocabulary, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", class="variety", verbose = TRUE) {
	setwd(path)

	#Cargamos el fichero de training
	truth <- read.csv("truth.txt", sep=":", header=FALSE)
	truth <- truth[,c(1,4,7)]
	colnames(truth) <- c("author", "gender", "variety")

  #Lista de ficheros que hay en la variables files
	i <- 0
	bow <- NULL
	files = list.files(pattern="*.xml")
  
  #Recorer los ficheros
	for (file in files) {
		author <- gsub(".xml", "", file)
		variety <- truth[truth$author==author,"variety"]
		gender <- truth[truth$author==author,"gender"]

		xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
		txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
		

		if (lowcase) {
			txtdata <- tolower(txtdata)
		}

		if (punctuations) {
			txtdata <- removePunctuation(txtdata)
		}

		if (numbers) {
			txtdata <- removeNumbers(txtdata)
		}

		if (whitespaces) {
			txtdata <- stripWhitespace(txtdata)
		}
	
		line <- author
		freq <- freq_terms(txtdata, n)
		for (word in vocabulary$WORD) {
			thefreq <- 0
			if (length(freq[freq$WORD==word,"FREQ"])>0) {
				thefreq <- freq[freq$WORD==word,"FREQ"]
			} 
			line <- paste(line, ",", thefreq, sep="")
		}
		if (class=="variety") {
			#line <- paste(line, ",", variety, sep="")
		  line <- paste(variety, ",",line,sep="")
		} else {
			#line <- paste(line, ",", gender, sep="")
		  line <- paste(gender, ",", line, sep="")
		}

		bow <- rbind(bow, line)

		i <- i + 1

		if (verbose) {
			if (class=="variety") {
				print(paste(i, author, variety))
			} else {
				print(paste(i, author, gender))
			}
		}
	}

	return (bow)
}


n <- 1000

path_training <- "/media/anberbai@alumno.upv.es/NIOAN7 32GB/Master/Asignaturas/16. Text Mining en Social Media/2017/pan-ap17-bigdata/training"
path_test <- "/media/anberbai@alumno.upv.es/NIOAN7 32GB/Master/Asignaturas/16. Text Mining en Social Media/2017/pan-ap17-bigdata/test"


vocabulary <- GenerateVocabulary(path_training, n, swlang="es")

bow_training <- GenerateBoW(path_training, vocabulary, n, class="variety") #gender variety
bow_test <- GenerateBoW(path_test, vocabulary, n, class="variety")

training <- cSplit(bow_training, "V1", ",")
test <- cSplit(bow_test, "V1", ",")

training1 <- training[,1]
training2 <- training[,3:ncol(training)]
#training2 <- training[,ncol(training)]
training <- cbind(training1,training2)

names(training)[1] <- "class" #columna 1008, es la clase com por ejemplo venezuela training[1, 1008]
truth  <- unlist(test[,1]) #valores de sexo o variedad de la clase de verdad.
test <- test[,3:ncol(test)]

#Aprendiendo y evaluando con validacion cruzada.
#train_control <- trainControl( method="repeatedcv", number = 10 , repeats = 3) 
#model_SVM <- train( class~., data= training, trControl = train_control, method = "svmLinear")
#print(model_SVM)

#or 

## All subsequent models are then run in parallel
model_SVM <- train(class ~ ., data = training, method = "rf", ntree=100)
print(model_SVM)

#Aprendiendo con todo el training
#train_control <- trainControl(method="none")
#model_SVM <- train( class~., data= training, trControl = train_control, method = "svmLinear")
#print(model_SVM)
#predictores son caracteristicas, es decir, la "n"
#clases.
#Accurasy es variety
#kappa mas cercano a 0, significa que es debido al azar.

#Evaluando con el test
pred_SVM <- predict(model_SVM, test)
confusionMatrix(pred_SVM, truth)



#######
#training <- training[,3:(n+3)]
#names(training)[n+1] <- "class" #columna 1008, es la clase como por ejemplo venezuela training[1, 1008]
#truth  <- unlist(test[,(n+3):(n+3)]) #valores de sexo o variedad de la clase de verdad.
#######

#######
#training <- cSplit(bow_training, "V1", ",")
#test <- cSplit(bow_test, "V1", ",")
#
#training <- training[,3:1003]
#test <- test[,1:1002]
#pred_SVM <- predict(model_SVM, test)
#confusionMatrix(pred_SVM, truth)
####### 



#corpus.frequentterms >- freq_terms(corpus.raw,n)
#corpus.frequenc
#plot(corpus.frequentterms)
  

