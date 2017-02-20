convertToReversedVector <- function (s) {
    strsplit(s, split = '')[[1]]
}

check <- function () {
    o <- 0
    
    for (i in 1:check_length) {
        if (letter[ arg_a[i] ] == -1 || letter[ arg_b[i] ] == -1 || letter[ arg_r[i] ] == -1) {
            return (T)
        }
        
        s <- o + letter[ arg_a[i] ] + letter[ arg_b[i] ]
        o <- s %/% 16
        s <- s %% 16
        
        if (s != letter[ arg_r[i] ]) {
            print(letter[1:i])
            return (F)
        }
    }
    T
}

backtrack <- function(p) {
    if ( !check() ) {
        return()
    }
    
    if (p > letter_length) {
        mega_check()
        stop()
        return()
    }
    
    for (n in 0:15) {
        np1 <- n + 1
        if (!used[np1]) {
            letter[p] <<- n
            used[np1] <<- T
            backtrack(p + 1)
            used[np1] <<- F
            letter[p] <<- -1
        }
    }
}

mega_check <- function () {
    print('===========')
    print(letter)
    
    mega_check_s <- convertToReversedVector('OPENER')
    result <- ''
    for (i in 1:length(mega_check_s)) {
        result <- paste0(result, as.hexmode(letter[ mega_check_s[i] ]))
    }
    print(result)
}

arg_a = rev(convertToReversedVector('GREATPEOPLE'))
arg_b = rev(convertToReversedVector('ITRANSITION'))
arg_r = rev(convertToReversedVector('DEVELOPMENT'))
check_length = length(arg_r)

used <- logical(16)

letter <- c(
    'E' = -1,
    'N' = -1,
    'T' = -1,
    'L' = -1,
    'O' = -1,
    'P' = -1,
    'I' = -1,
    'M' = -1,
    'S' = -1,
    'A' = -1,
    'R' = -1,
    'V' = -1,
    'G' = -1,
    'D' = -1
)
letter_length = length(letter)

backtrack(1)

# result 9a242c = 10101804