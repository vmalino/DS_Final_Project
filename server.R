library(shiny)
library(dplyr)

# 2-grams of 3 corpuses

b2grams <- as.data.frame(rbind(
        c("it","could","3"),
        c("a","lot","2"),
        c("a","bottle","2"),
        c("an","impetus","2"),
        c("and","they","2"),
        c("be","a","2")
), stringsAsFactors = FALSE)

n2grams <- as.data.frame(rbind(
        c("it","could","3"),
        c("a","lot","2"),
        c("a","bottle","2"),
        c("an","impetus","2"),
        c("and","they","2"),
        c("be","a","2")
), stringsAsFactors = FALSE)

t2grams <- as.data.frame(rbind(
        c("it","could","3"),
        c("a","lot","2"),
        c("a","bottle","2"),
        c("an","impetus","2"),
        c("and","they","2"),
        c("be","a","2")
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
        c("a","couple","of","128")
), stringsAsFactors = FALSE)

n3grams <- as.data.frame(rbind(
        c("one","of","the","337"),
        c("a","lot","of","300"),
        c("as","well","as","153"),
        c("the","end","of","150"),
        c("some","of","the","144"),
        c("out","of","the","139"),
        c("i","want","to","135"),
        c("be","able","to","130"),
        c("a","couple","of","128")
), stringsAsFactors = FALSE)

t3grams <- as.data.frame(rbind(
        c("one","of","the","337"),
        c("a","lot","of","300"),
        c("as","well","as","153"),
        c("the","end","of","150"),
        c("some","of","the","144"),
        c("out","of","the","139"),
        c("i","want","to","135"),
        c("be","able","to","130"),
        c("a","couple","of","128")
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