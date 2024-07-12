board <- function(DIM, N_MINES){
  A <- matrix(0, DIM, DIM)
  MINE_P <- sample(1:(DIM^2), N_MINES)
  A[(MINE_P)] <- 1
  return(A)
}

openboard <- function(DIM, N_MINES, HOLE){
  A <- matrix(0, DIM, DIM)
  B <- setdiff(1:DIM^2, c(HOLE,adjacents(DIM, HOLE)))
  MINE_P <- sample(B, N_MINES)
  A[(MINE_P)] <- 1
  return(A)
}

adjacentsum <- function(A){
  D <- nrow(A)

  Aleft <- cbind(0, A[, 1:(D-1)])
  Aright <- cbind(A[, 2:D], 0)
  Aup <- rbind(A[2:D, ], 0)
  Adown <- rbind(0, A[1:(D-1),])

  return(A + Aleft + Aright + Aup + Adown)
}

diagnolsum <- function(A){
  D <- nrow(A)

  Adownleft <- rbind(0, cbind(A[1:(D-1), 2:D], 0))
  Aupright <- cbind(0, rbind(A[2:D, 1:(D-1)],0))
  Aupleft <- cbind(rbind(A[2:D, 2:D], 0), 0)
  Adownright <- cbind(0, rbind(0, A[1:(D-1), 1:(D-1)]))

  return(A + Adownleft + Aupright + Aupleft + Adownright)
}

power <- function(A, base){
  B <- base^A
  B[B==1] <- 0
  return(B)
}

minesum <- function(A) return(A * adjacentsum(A))

cells <- function(A) return(as.numeric(A==0)*(adjacentsum(A) + diagnolsum(A)))

rcindex <- function(dim, i) {
  row <- (i - 1) %/% dim + 1
  col <- (i - 1) %% dim + 1
  return(c(row, col))
}

index <- function(row, col, dim) return( (col - 1) * dim + row)

adjacents <- function(DIM, cell) {
  rc <- rcindex(DIM, cell)

  row_offsets <- c(-1, 0, 1, -1, 1, -1, 0, 1)
  col_offsets <- c(-1, -1, -1, 0, 0, 1, 1, 1)

  adjacent_rows <- pmin(pmax(rc[1] + row_offsets, 1), DIM)
  adjacent_cols <- pmin(pmax(rc[2] + col_offsets, 1), DIM)

  adjacent_indexes <- sort(unique((adjacent_rows - 1) * DIM + adjacent_cols))
  return(adjacent_indexes)
}

adjacentA <- function(cell, A){
  adj <- adjacents(dim(A)[1], cell)
  adjA <- adj[adj %in% which(A)]
  adjA <- setdiff(adjA, c(cell))
  return(adjA)
}

dig <- function(H, HOLES, visited=NULL, notvisited=NULL){
  visited <- c(visited, H)

  adj <- adjacentA(H, HOLES)
  notvisited <- unique(c(notvisited, setdiff(adj, visited)))

  if (length(notvisited)==0){
    return(sort(unique(visited)))
  }

  for (h in notvisited){
    visited <- c(visited, h)
    notvisited <- setdiff(notvisited, h)
    return(dig(h, HOLES, visited, notvisited))
  }

}

pit <- function(H, HOLES, CELLS){

  ADJHOLES <- dig(H, HOLES)
  ADJCELLS <- unique(unlist(lapply(ADJHOLES, adjacentA,CELLS!=0)))
  return(sort(unique(c(ADJHOLES, ADJCELLS))))
}

dtboard <- function(M){
  DT <- data.table(X=rep(1:nrow(M), each=ncol(M)),
                   Y=rep(1:ncol(M), times=nrow(M)),
                   L=as.vector(M))
  return(DT)
}

# replay <- function(){
#   r <- menu(c("Yes", "No"), title="Play again?")
#   if (r==1){
#     game()
#   } else{
#     message("Carry on ...")
#   }
# }

# text_game <- function(){
#
#     DIM <<- as.numeric(readline("Enter Dimension: "))
#     N_MINES <<- as.numeric(readline("Enter Number of Mines: "))
#     BASE <<- as.numeric(readline("Enter Base: "))
#     message("Enter row, column (\"F\" to flag, \"C\" to chord)")
#
#     BOARD <<- matrix("", DIM, DIM)
#     FIRST <<- TRUE
#     OPEN <<- NULL
#     FLAGS <<- matrix(FALSE, DIM, DIM)
#
#     running=TRUE
#     while (running){
#
#       plot_board(BOARD, CELLS)
#       FLAG <<- FALSE
#       CHORD <<- FALSE
#
#       rp <<- function() {
#         input <- readline("")
#
#         if (input=="F"){
#           message("Place flag")
#           input <- readline("")
#           FLAG <<- TRUE
#         }
#
#         if (input=="C"){
#           message("Open all neighbors")
#           input <- readline("")
#           CHORD <<- TRUE
#         }
#
#         input_values <- unlist(strsplit(input, "[,\\s]+"))
#         move <<- c(as.numeric(input_values[1]), as.numeric(input_values[2]))
#         i <<- index(move[1], move[2], DIM)
#       }
#
#       play <<- function() {
#         if (FIRST){
#           if (BASE==1){
#             MINES <<- minesum(openboard(DIM, N_MINES, i))
#           } else{
#             MINES <<- power(minesum(openboard(DIM, N_MINES, i)), BASE)
#           }
#           message("ROW/COL ADJACENT MINES ADD!")
#           CELLS <<- cells(MINES)
#           iMINES <<- which(MINES!=0)
#           iCELLS <<- which(CELLS!=0)
#           HOLES <<- CELLS==0 & MINES==0
#           OPEN <<- c(OPEN, i)
#           FIRST <<- FALSE
#         }
#
#
#         if (FLAG){
#           FLAGS[i] <<- TRUE
#           BOARD[i] <<- "F"
#         } else if (CHORD) {
#           ichord <- c(i,setdiff(adjacentA(i, MINES==0), OPEN))
#           ichordmines <- adjacentA(i, MINES!=0)
#           BOARD[ichord] <<- CELLS[ichord]
#           if (length(ichordmines)>0){
#             BOARD[ichordmines] <<- "X"
#             message("KABOOOOOM!")
#             plot_board(BOARD, CELLS)
#             running=FALSE
#             replay()
#           }
#         } else if (i %in% iMINES) {
#           BOARD[i] <<- "X"
#           message("KABOOOOOM!")
#           plot_board(BOARD, CELLS)
#           running=FALSE
#           replay()
#         } else{
#           if (HOLES[i]){
#             ipit <- pit(i, HOLES, CELLS)
#             OPEN <<- c(OPEN, ipit)
#             BOARD[ipit] <<- CELLS[ipit]
#           } else{
#             BOARD[i] <<- CELLS[i]
#           }
#
#           OPEN <<- c(OPEN, i)
#
#           if (all(iCELLS %in% OPEN)){
#             message("WINNER!")
#             plot_board(BOARD, CELLS)
#             running=FALSE
#             replay()
#           }
#         }
#       }
#
#       tryCatch(
#         expr = {
#           rp()
#           play()
#         },
#         error = function(e){
#           rp()
#           play()
#         }
#       )
#     }
# }


