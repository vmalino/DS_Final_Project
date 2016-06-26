library(shiny)
library(dplyr)

# 2-grams of 3 corpuses

b2grams <- as.data.frame(rbind(
        c("of","the","4160"),
        c("in","the","3453"),
        c("to","the","1922"),
        c("on","the","1633"),
        c("to","be","1503"),
        c("and","the","1321"),
        c("for","the","1245"),
        c("and","i","1084"),
        c("i","was","1082"),
        c("is","a","1047")
), stringsAsFactors = FALSE)

n2grams <- as.data.frame(rbind(
        c("of","the","14050"),
        c("in","the","13496"),
        c("to","the","6408"),
        c("on","the","5543"),
        c("for","the","5379"),
        c("at","the","4489"),
        c("and","the","4024"),
        c("in","a","4003"),
        c("to","be","3547"),
        c("with","the","3318")
), stringsAsFactors = FALSE)

t2grams <- as.data.frame(rbind(
        c("in","the","3369"),
        c("for","the","3184"),
        c("of","the","2371"),
        c("on","the","2005"),
        c("to","be","1957"),
        c("to","the","1891"),
        c("thanks","for","1866"),
        c("at","the","1540"),
        c("i","love","1497"),
        c("going","to","1423")
), stringsAsFactors = FALSE)

# 3-grams of 3 corpuses

b3grams <- as.data.frame(rbind(
        c("one","of","the","337"),
        c("a","lot","of","300"),
        c("as","well","as","153"),
        c("the","end","of","150"),
        c("some","of","the","144"),
        c("out","of","the","139"),
        c("i","want","to","135"),
        c("be","able","to","130"),
        c("a","couple","of","128"),
        c("to","be","a","128")
), stringsAsFactors = FALSE)

n3grams <- as.data.frame(rbind(
        c("one","of","the","526"),
        c("a","lot","of","468"),
        c("as","well","as","241"),
        c("according","to","the","238"),
        c("in","the","first","226"),
        c("the","end","of","221"),
        c("going","to","be","209"),
        c("out","of","the","199"),
        c("part","of","the","193"),
        c("to","be","a","188")
), stringsAsFactors = FALSE)

t3grams <- as.data.frame(rbind(
        c("thanks","for","the","1018"),
        c("looking","forward","to","384"),
        c("thank","you","for","374"),
        c("cant","wait","to","358"),
        c("i","love","you","344"),
        c("for","the","follow","338"),
        c("i","want","to","323"),
        c("going","to","be","318"),
        c("a","lot","of","267"),
        c("i","need","to","251")
), stringsAsFactors = FALSE)

# Prediction algorithm

predby1 <- function(word, corps) {
        bresp <- NULL
        nresp <- NULL
        tresp <- NULL
        if (1 %in% corps) bresp <- b2grams[b2grams$V1 == word,]
        if (2 %in% corps) nresp <- n2grams[n2grams$V1 == word,]
        if (3 %in% corps) tresp <- t2grams[t2grams$V1 == word,]
        resp <- rbind(bresp[1:3,], nresp[1:3,], tresp[1:3,])
        resp <- resp[!is.na(resp$V3),]
        if (nrow(resp) == 0) {
                return(NULL)
        } else {
                resp$V3 <- as.integer(resp$V3)
                resp <- resp %>% group_by(V2) %>% summarize(w = sum(V3)) %>%
                        arrange(desc(w))
                return(resp[1:3, 1])
        }
}

predby2 <- function(word1, word2, corps) {
        bresp <- NULL
        nresp <- NULL
        tresp <- NULL
        if (1 %in% corps) bresp <- b3grams[b3grams$V1 == word1 & b3grams$V2 == word2,]
        if (2 %in% corps) nresp <- n3grams[n3grams$V1 == word1 & n3grams$V2 == word2,]
        if (3 %in% corps) tresp <- t3grams[t3grams$V1 == word1 & t3grams$V2 == word2,]
        resp <- rbind(bresp[1:3,], nresp[1:3,], tresp[1:3,])
        resp <- resp[!is.na(resp$V4),]
        if (nrow(resp) == 0) {
                return(NULL)
        } else {
                resp$V4 <- as.integer(resp$V4)
                resp <- resp %>% group_by(V3) %>% summarize(w = sum(V4)) %>%
                        arrange(desc(w))
                return(resp[1:3, 1])
        }
}

top3pred <- function(word1st, word2nd, selcorps) {
        if (length(word2nd) == 0) {
                return(predby1(word1st, selcorps))
        } else {
                if(!is.null(predby2(word1st, word2nd, selcorps))) {
                        return(predby2(word1st, word2nd, selcorps))
                } else {
                        return(predby1(word2nd, selcorps))
                }
        }
}

# Service functions

replace

# Main server function

shinyServer(
        function(input, output, clientData, session) {
                output$pred = renderUI({
                        selectedCorps <- input$corps
                        input <- strsplit(input$inp, " ")[[1]]
                        len <- length(input)
                        if (len == 0) {
                                HTML("Nothing is entered!")
                        } else {
                                res <- top3pred(tolower(input[len-1]),
                                                tolower(input[len]),
                                                selectedCorps)
                                if (is.null(res)) {
                                        HTML("Nothing is found!")
                                } else {
                                        res[is.na(res)] <- ""
                                        HTML(paste(res[1, 1], res[2, 1], res[3, 1],
                                                   sep = '<br/>'))
                                }
                        }
                })
        }
)