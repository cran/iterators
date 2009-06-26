#
# Copyright (c) 2008-2009, REvolution Computing, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# generic function for creating an iterator object
iter <- function(obj, ...) {
  UseMethod('iter')
}

# calling iter on an iter object returns itself
iter.iter <- function(obj, ...) {
  obj
}

# default method creates an iterator from a vector or list
iter.default <- function(obj, checkFunc=function(...) TRUE, recycle=FALSE, ...) {
  state <- new.env()
  state$i <- 0L
  state$obj <- obj
  it <- list(state=state, checkFunc=checkFunc, recycle=recycle)
  class(it) <- c('containeriter', 'iter')
  it
}

# allow a matrix to be iterated over in different ways
iter.matrix <- function(obj, by=c('column', 'cell', 'row'), chunksize=1L, checkFunc=function(...) TRUE,
                        recycle=FALSE, ...) {
  by <- match.arg(by)
  if((chunksize > 1L) && (by=='cell')) {
    warning("Chunksize greater than 1 not allowed when using by='cell'\n  Setting chunksize=1")
    chunksize <- 1L
  }
  state <- new.env()
  state$i <- 0L
  state$obj <- obj
  it <- list(state=state, by=by, checkFunc=checkFunc, recycle=recycle, chunksize=chunksize)
  class(it) <- c('matrixiter', 'iter')
  it
}

# allow a data frame to be iterated over in different ways
iter.data.frame <- function(obj, by=c('column', 'row'), checkFunc=function(...) TRUE,
                            recycle=FALSE, ...) {
  by <- match.arg(by)
  state <- new.env()
  state$i <- 0L
  state$obj <- obj
  it <- list(state=state, by=by, length=switch(by, row=nrow(obj), length(obj)),
             checkFunc=checkFunc, recycle=recycle)
  class(it) <- c('dataframeiter', 'iter')
  it
}

# allow a closure to be turned into an iterator object
iter.function <- function(obj, checkFunc=function(...) TRUE,
                          recycle=FALSE, ...) {
  state <- new.env()
  state$i <- 0L
  state$fun <- obj
  args <- !is.null(formals(obj))
  it <- list(state=state, args=args, checkFunc=checkFunc)
  class(it) <- c('funiter', 'iter')
  it
}

getIterVal <- function(obj, plus, ...) {
  UseMethod('getIterVal')
}

getIterVal.containeriter <- function(obj, plus=0L, ...) {
  return(obj$state$obj[[obj$state$i+plus]])
}

getIterVal.matrixiter <- function(obj, plus=0L, ...) {
  return(switch(obj$by,
         column = obj$state$obj[,
           (obj$state$i+plus):
           min(obj$state$i+plus+(obj$chunksize-1L),ncol(obj$state$obj)),
           drop=FALSE],
         row = obj$state$obj[(obj$state$i+plus):
           min(obj$state$i+plus+(obj$chunksize-1L),nrow(obj$state$obj)), ,
           drop=FALSE],
         obj$state$obj[[obj$state$i+plus]]))
}

getIterVal.dataframeiter <- function(obj, plus=0L, check=TRUE, ...) {
  if ((obj$state$i + plus) > obj$length)
    stop('StopIteration',call.=FALSE)
  return(switch(obj$by,
         row=obj$state$obj[obj$state$i+plus, ],
         obj$state$obj[, obj$state$i+plus]))
}

nextElem <- function(obj, ...) {
  UseMethod('nextElem')
}

nextElem.containeriter <- function(obj, ...) {
  repeat {
    tryCatch({
      if (obj$checkFunc(getIterVal(obj,1L))) {
        obj$state$i <- obj$state$i + 1L
        return(getIterVal(obj))
      }
      obj$state$i <- obj$state$i + 1L
    }, error=function(e) {
      if (any(nzchar(e$message))) {
        if (identical(e$message, "subscript out of bounds")) {
          if (obj$recycle) {
            obj$state$i <- 0L
          }
          else {
            stop('StopIteration', call.=FALSE)
          }
        }
        else {
          stop(e$message, call.=FALSE)
        }
      }
      else {
        stop('Abort', call.=e)
      }
    })
  }
}

nextElem.matrixiter <- function(obj, ...) {
  repeat {
    tryCatch({
      if (obj$checkFunc(getIterVal(obj,1L))) {
        obj$state$i <- obj$state$i + obj$chunksize
        return(getIterVal(obj,plus=(1L-obj$chunksize)))
      }
      obj$state$i <- obj$state$i + obj$chunksize
    }, error=function(e) {
      if (any(nzchar(e$message))) {
        if (identical(e$message, "subscript out of bounds") ||
            identical(e$message, "attempt to select more than one element")) {
          if (obj$recycle) {
            obj$state$i <- 0L
          }
          else {
            stop('StopIteration', call.=FALSE)
          }
        }
        else {
          stop(e$message, call.=FALSE)
        }
      }
      else {
        stop('Abort', call.=e)
      }
    })
  }
}

nextElem.dataframeiter <- function(obj, ...) {
  repeat {
    tryCatch({
      if (obj$checkFunc(getIterVal(obj,1L))) {
        obj$state$i <- obj$state$i + 1L
        return(getIterVal(obj))
      }
      obj$state$i <- obj$state$i + 1L
    }, error=function(e) {
      if (any(nzchar(e$message))) {
        if (identical(e$message, "StopIteration")) {
          if (obj$recycle) {
            obj$state$i <- 0L
          }
          else {
            stop('StopIteration', call.=FALSE)
          }
        }
        else {
          stop(e$message, call.=FALSE)
        }
      }
      else {
        stop('Abort', call.=e)
      }
    })
  }
}

nextElem.funiter <- function(obj, ...) {
  repeat {
    tryCatch({
      if (obj$args) {
        val <- obj$state$fun(obj$state$i+1L)
      }
      else {
        val <- obj$state$fun()
      }
      if (obj$checkFunc(val)) {
        if (obj$args) obj$state$i <- obj$state$i + 1L
        return(val)
      }
      if (obj$args) obj$state$i <- obj$state$i + 1L
    }, error=function(e) {
      if (any(nzchar(e$message))) {
        if (identical(e$message, "StopIteration")) {
          if (obj$recycle) {
            if (obj$args) obj$state$i <- 0L
          }
          else {
            stop('StopIteration', call.=FALSE)
          }
        }
        else {
          stop(e$message, call.=FALSE)
        }
      }
      else {
        stop('Abort', call.=e)
      }
    })
  }
}

nextElem.abstractiter <- function(obj, ...) {
  obj$nextElem()
}

#print.containeriter <- function(x, ...) {
#  repr <- sprintf('<%s iterator, current value %d\n',
#                  class(x$state$obj)[1], getIterVal(x))
#  cat(repr)
#}

#print.matrixiter <- function(x, ...) {
#  repr <- sprintf('<%s iterator, current value %d\n',
#                  class(x$state$obj)[1], getIterVal(x))
#  cat(repr)
#}

#print.dataframeiter <- function(x, ...) {
#  repr <- sprintf('<%s iterator, current value %d\n',
#                  class(x$state$obj)[1], getIterVal(x, check=FALSE))
#  cat(repr)
#}

#print.funiter <- function(x, ...) {
#  cat('function iterator\n')
#}

#print.abstractiter <- function(x, ...) {
#  cat(x$toString())
#}
