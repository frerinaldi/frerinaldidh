Coordinates <- setRefClass("Coordinates",
                           fields=list(
                             col = "numeric",
                             row = "numeric",
                             # isValid is true if - and only if - the position
                             # (col,row) is a valid coordinate in the chessboard
                             isValid = "logical"
                           ),
                           methods=list(
                             initialize = function(newCol, newRow) {
                               col <<- newCol
                               row <<- newRow
                               # We need to check that col and row are valid
                               # coordinates for the chessboard (so >= 1 and <= 8)
                               if ((col >= 1) && (col <= 8) && 
                                   (row >= 1) && (row <= 8)) {
                                 isValid <<- TRUE
                               } else {
                                 isValid <<- FALSE
                               }
                             }
                           ))

# The class Piece identifies a piece of the game that could be white or black; 
# Objects of type Piece exist only into the matrix defined in the Chessboard 
# class.

Pawn <- setRefClass("Piece",
                    fields=list(
                      color = "character", # w - white, b - black
                      isPiece = "logical"
                    ),
                    methods=list(
                      initialize = function(newColor) {
                        # We want to check if the newColor passed as parameter 
                        # is 'w' or 'b'; If the value is not valid, the 
                        # execution of the software is stopped
                        if ((newColor != "w") && (newColor != "p")) {
                          stop("invalid parameters to the ctor (", newColor, 
                               " but only 'b' and 'w' are valid)")
                        } else {
                          color <<- newColor
                          isPiece <<- FALSE
                        }}
                      ,
                      checkTrajectory=function(source, destination, currentColor, cb) {
                        return (TRUE)
                      },
                      #defines the move this piece can do
                      checkMove=function(source,destination, currentColor, cb) {
                        if ((destination$row-source$row) == 1) {
                          if (destination$col==source$col) {
                            return(TRUE)
                          }
                          
                        } 
                        # else if (cb[source$row, source$col][[1]]$currentColor == "b") {
                        #     if (source$row==7) { 
                        #      if ((source$row-destination$row) == 2 || (destination$col-source$col) ==1) {
                        #       return(TRUE)
                        #    }}
                        #     if ((destination$row-source$row) == -1) {
                        #pawn - eat
                        #      if ((is.null(cb[destination$row, destination$col][[1]]) == FALSE) &&
                        #         (cb[destination$row, destination$col][[1]]$color != currentColor)) { 
                        #       if (abs(destination$col-source$col)<=1) {
                        #        return(TRUE)
                        #       } #default move
                        #      else if (destination$col==source$col) {
                        #       return(TRUE)
                        #     }
                        #   }
                        #}
                        return(FALSE)
                      }
                    ))
King <- setRefClass("King",
                    fields=list(
                      color = "character", # w - white, b - black
                      isKing = "logical"
                    ),
                    methods=list(
                      initialize = function(newColor) {
                        # We want to check if the newColor passed as parameter 
                        # is 'w' or 'b'; If the value is not valid, the 
                        # execution of the software is stopped
                        if ((newColor != "w") && (newColor != "p")) {
                          stop("invalid parameters to the ctor (", newColor, 
                               " but only 'b' and 'w' are valid)")
                        } else {
                          color <<- newColor
                          isKing <<- FALSE
                        }
                      },
                      checkTrajectory=function(source, destination, currentColor, cb) {
                        return (TRUE)
                      },
                      #defines the move this piece can do
                      checkMove=function(source,destination, cb, currentColor) {
                        if (abs(destination$row-source$row)<=1) {
                          #default move of the king
                          if (abs(destination$col-source$col)<=1) {
                            # MOSSA VALIDA
                            return(TRUE)
                          }
                        }
                        # MOSSA NON VALIDA
                        return(FALSE)
                      }
                    ))
Rook <- setRefClass("Rook",
                    fields=list(
                      color = "character", # w - white, b - black
                      isRook = "logical"
                    ),
                    methods=list(
                      initialize = function(newColor) {
                        # We want to check if the newColor passed as parameter 
                        # is 'w' or 'b'; If the value is not valid, the 
                        # execution of the software is stopped
                        if ((newColor != "w") && (newColor != "p")) {
                          stop("invalid parameters to the ctor (", newColor, 
                               " but only 'b' and 'w' are valid)")
                        } else {
                          color <<- newColor
                          isRook <<- FALSE
                        }
                      },
                      checkTrajectory=function(source, destination, currentColor, cb) {
                        if ((source$col==destination$col) && (abs(source$row-destination$row)>1))  {
                          if (source$row > destination$row) {
                            temp<-source$row
                            source$row<-destination$row
                            destination$row<-temp
                          }
                          for (i in seq (source$row+1, destination$row-1)) {
                            if ((is.null(cb[i, source$col][[1]]) == FALSE)) 
                            {return(FALSE)}}}
                        else if ((source$row==destination$row) && (abs(source$col-destination$col)>1)) { 
                          if (source$col > destination$col) {
                            temp<-source$col
                            source$col <-destination$col
                            destination$col <-temp}
                          for (i in seq (source$col + 1, destination$col - 1)) {
                            if ((is.null(cb[i, source$row][[1]]) == FALSE)) {
                              # if (((is(cb[source$row, i][[1]], "Piece") == TRUE)) || ((is(cb[source$row, i][[1]], "Rook") == TRUE)) || ((is(cb[source$row,i][[1]], "King") == TRUE)) || ((is(cb[source$row,i][[1]], "Bishop") == TRUE)) || ((is(cb[source$row,i][[1]], "Queen") == TRUE)) || ((is(cb[source$row,i][[1]], "Knight") == TRUE))) {
                              return(FALSE)
                            }
                          }
                        }
                        return (TRUE)
                      },
                      #defines the move this piece can do
                      checkMove=function(source,destination, cb, currentColor) {
                        if (((abs(destination$row-source$row)<=8) && (destination$col==source$col)) || (((destination$row==source$row)) && ((abs(destination$col-source$col))<=8 )) ) {
                          # MOSSA VALIDA
                          return(TRUE)
                        }
                        # MOSSA NON VALIDA
                        return(FALSE)
                      }
                    ))


Bishop <- setRefClass("Bishop",
                      fields=list(
                        color = "character", # w - white, b - black
                        isBishop = "logical"
                      ),
                      methods=list(
                        initialize = function(newColor) {
                          # We want to check if the newColor passed as parameter 
                          # is 'w' or 'b'; If the value is not valid, the 
                          # execution of the software is stopped
                          if ((newColor != "w") && (newColor != "p")) {
                            stop("invalid parameters to the ctor (", newColor, 
                                 " but only 'b' and 'w' are valid)")
                          } else {
                            color <<- newColor
                            isBishop <<- FALSE
                          }
                        },
                        checkTrajectory=function(source, destination, currentColor,cb) {
                          absDist <- as.integer((abs(destination$row-source$row)))
                          if (destination$row > source$row) {
                            if (destination$col > source$col) {
                              for (i in seq(1, absDist, by=1)){
                                if  ((is.null(cb[source$col + i, source$row + i][[1]]) == FALSE)) {
                                  return(FALSE)
                                }}}
                            else if (destination$col < source$col){
                              for (i in seq(1, absDist, by=1)){
                                if  ((is.null(cb[source$col - i, source$row + i][[1]]) == FALSE)) {
                                  return(FALSE)
                                }}}}
                          else if (destination$row < source$row){if (destination$col > source$col) {
                            for (i in seq(1, absDist, by=1)){
                              if  ((is.null(cb[source$col + i, source$row - i][[1]]) == FALSE)) {
                                return(FALSE)
                              }}}
                            else if (destination$col < source$col){
                              for (i in seq(1, absDist, by=1)){
                                if  ((is.null(cb[source$col - i, source$row - i][[1]]) == FALSE)) {
                                  return(FALSE)
                                }}}}
                          return(TRUE)},
                        #defines the move this piece can do
                        checkMove=function(source, destination, cb, currentColor) {
                          if ((abs(destination$col-source$col)) == (abs(destination$row-source$row))) {
                            # MOSSA VALIDA
                            return(TRUE)
                          }
                          # MOSSA NON VALIDA
                          return(FALSE)
                        }
                      ))

Queen <- setRefClass("Queen",
                     fields=list(
                       color = "character", # w - white, b - black
                       isQueen = "logical"
                     ),
                     methods=list(
                       initialize = function(newColor) {
                         # We want to check if the newColor passed as parameter 
                         # is 'w' or 'b'; If the value is not valid, the 
                         # execution of the software is stopped
                         if ((newColor != "w") && (newColor != "p")) {
                           stop("invalid parameters to the ctor (", newColor, 
                                " but only 'b' and 'w' are valid)")
                         } else {
                           color <<- newColor
                           isQueen <<- FALSE
                         }
                       },
                       checkTrajectory=function(source, destination, currentColor) {
                         if (source$col==destination$col) {
                           for (i in seq (source$Row + 1, destination$Row - 1, by=1)) {
                             if ((is(cb[i, source$col][[1]], "Piece") == TRUE)){
                               return(FALSE)
                             }
                           }
                         }
                         else if (source$row==destination$row) { 
                           for (i in seq (destination$Row + 1, source$Row - 1, by=1)) {
                             if ((is(cb[i, source$col][[1]], "Piece") == TRUE)){
                               return(FALSE)
                             }
                           }
                         }
                         return (TRUE)
                       },
                       #defines the move this piece can do
                       checkMove=function(source, destination, cb, currentColor) {
                         if (((abs(destination$row-source$row)<=8) && ((abs(destination$col-source$col))==0 )) || ((abs(destination$col-source$col)==0) && ((abs(destination$row-source$row))<=8 ) ) || ((abs(destination$col-source$col)) == (abs(destination$row-source$row)))) {
                           # MOSSA VALIDA
                           return(TRUE)
                         }
                         # MOSSA NON VALIDA
                         return(FALSE)
                       }
                     ))

Knight <- setRefClass("Knight",
                      fields=list(
                        color = "character", # w - white, b - black
                        isKnight = "logical"
                        
                      ),
                      methods=list(
                        
                        
                        initialize = function(newColor) {
                          # We want to check if the newColor passed as parameter 
                          # is 'w' or 'b'; If the value is not valid, the 
                          # execution of the software is stopped
                          if ((newColor != "w") && (newColor != "p")) {
                            stop("invalid parameters to the ctor (", newColor, 
                                 " but only 'b' and 'w' are valid)")
                          } else {
                            color <<- newColor
                            isKnight <<- FALSE
                          }
                        },
                        checkTrajectory=function(source, destination, currentColor) {
                          return (TRUE)
                        },
                        #defines the move this piece can do
                        checkMove=function(source, destination, cb, currentColor) {
                          if ((((((abs(destination$row-source$row)) == 2) && (abs(destination$col-source$col)) == 1)) || (((abs(destination$col-source$col)) == 1) && (abs(destination$row-source$row)) == 2)))  {
                            # MOSSA VALIDA
                            return(TRUE)
                          }
                          # MOSSA NON VALIDA
                          return(FALSE)
                        }
                      ))